---
title: "Using funflow to cache a nix based workflow"
---

My latest project has been to plot a [map of orienteering maps](http://mpickering.github.io/maps.html) in the UK. This post explains the technical aspects behind the project
and primarily the use of [`funflow`](https://hackage.haskell.org/package/funflow)
to turn my assortment of scripts into into a resumable workflow.

There was nothing wrong with my ad-hoc python and bash scripts
but they downloaded and regenerated the whole output every time. The whole
generation takes about 2 hours so it's desirable to only recompute the necessary
portions. This is where `funflow` comes in, by stringing together these scripts
in their DSL, you get caching for free. The workflow is also highly
parallelisable so in the future I could distribute the work across multiple machines
if necessary.

The code for the project can be found [here](https://github.com/mpickering/rg-map).

![](https://i.imgur.com/NVLui01.png){ style="width: 50%; margin: auto; display: block" }

<!--more-->

## `funflow`

There are already two blog posts introducing the concepts of `funflow`.

1. [Funflow: Typed Resumable Workflows](https://www.tweag.io/posts/2018-04-25-funflow.html)
2. [Funflow Example: Emulating Make](https://www.tweag.io/posts/2018-07-10-funflow-make.html)

The main idea is that you specify your workflow (usually a sequence of external scripts)
using a DSL and then `funflow` will automatically cache and schedule the
steps.

My primary motivation for using `funflow` was the automatic caching. The store
is content addressed which means that the location for each file in the store
depends on its contents. `funflow` performs two different types of caching.

1. Input-based caching: A flow will only be executed once for each set of inputs.
2. Output-based caching: If multiple different steps produce the same output then it will only be stored once in the store. Further steps will not be recomputed.

The lack of output-based caching is one of the big missing features of nix which
makes it unsuitable for this task. A content-addressed store where the address
depends on the contents of the file is sometimes known as an **intensional**
store. Nix's store model is **extensional** as the store hash only depends on the
inputs to the build.

An intensional store relies on the program producing deterministic output hashes.
It can be quite difficult to track down why a step is not being cached when you
are relying on the output's being identified in the store.

## High-level architecture

There are two outputs to the project.

1. A folder of map tiles rendered at different resolutions.
2. A HTML page which contains the javascript to display the map and markers.

This folder is then uploaded to online storage and served as a static site.

The processing pipeline is as follows:

1. Find all the maps with location information from [routegadget.co.uk](http://www.routegadget.co.uk/).
2. Download the metainformation and map image for each map.
3. Convert the maps to a common image format.
4. Reproject the maps to remove any rotation.
5. Merge overlapping maps into groups.
6. Generate tiles at all the different resolutions.
7. Combine all the tiles groups together.
8. Generate the website with the map and location markers.

As can be seen, the workflow is firstly highly parallelisable as much of the processing
pipeline happens independently of other steps. However, the main goal is to avoid
computing the tiles as much as possible as this is the step which takes by far the
longest.
At the time of writing there are about 500 maps to process.
In general, there are about 5-10 maps added each week. Only recomputing the changed
portions of the map saves a lot of time.

## Implementation using `funflow`

In theory, this is a perfect application for `funflow`
but in order to achieve the perfect caching
behaviour I had to rearchitecture several parts of the application.

### Using nix scripts
The recommended way to use `funflow` is to run each step of the flow in a docker
container. I didn't want to do this was my scripts already declared the correct
environment to run in by using the [`nix-shell` shebang](http://iam.travishartwell.net/2015/06/17/nix-shell-shebang/).

```
#! /usr/bin/env nix-shell
#! nix-shell -i bash -p gdal
```

By placing these two lines at the top of the file, the script will be run using
the `bash` interpreter with the `gdal` package available. This is more lightweight
and flexible than using a docker image as I don't have to regenerate a new docker image
any time I make a change.

However, there is no native support for running these kinds of scripts built into
`funflow`. It was easy enough to define my own function in order to run these
kinds of scripts using the `external'` primitive.

`nixScript` takes a boolean parameter indicating whether the script is pure and
should be cached. The name of the script to run, the names of any files the
script depends on and finally a function which supplies any additional
arguments to the script.

```
nixScriptX :: ArrowFlow eff ex arr => Bool
                                   -> Path Rel File
                                   -> [Path Rel File]
                                   -> (a -> [Param])
                                   -> arr (Content Dir, a) CS.Item
nixScriptX impure script scripts params = proc (scriptDir, a) -> do
  env <- mergeFiles -< absScripts scriptDir
  external' props (\(s, args) -> ExternalTask
        { _etCommand = "perl"
        , _etParams = contentParam (s ^</> script) : params args
        , _etWriteToStdOut = NoOutputCapture
        , _etEnv = [("NIX_PATH", envParam "NIX_PATH")] }) -< (env, a)
  where
    props = def { ep_impure = impure }
    absScripts sd = map (sd ^</>) (script : scripts)
```

The use of `perl` as the command relies on [the behaviour](http://perldoc.perl.org/perlrun.html) of `perl` that it
will execute the `#!` line if it does not contain the word "perl". Yes, this [is dirty](https://askubuntu.com/questions/850384/is-there-a-command-for-running-a-script-according-to-its-shebang-line/850387).

It would be desirable to set `NIX_PATH` to a fixed `nixpkgs` checkout by passing
a tarball directly but this worked for now.

All the steps are then defined in terms of `nixScriptX` indirectly as two helper
functions are defined for the two cases of a pure or impure scripts.

```
nixScript = nixScriptX False
impureNixScript = nixScriptX True
```

## Step 1 - Finding the map information

Now to the nitty gritty details.

Firstly, I had to decouple the processing of finding the map metainformation from
downloading the image. Otherwise, I would end up doing a lot of redundant work
downloading images multiple times.

The python script `scraper.py` executes a selenium driver to extract the map
information. For each map, the metainformation is serialised to its own file
in the output directory.

```
scrape = impureNixScript [relfile|scraper.py|] [[relfile|shell.nix|]]
          (\() -> [ outParam ])
```

This step is marked as impure as we have to run it every time the flow runs to
work out if we need to perform any more work.

It is important that the filename of the serialised information is the same
if the content of the file is the same. Otherwise, `funflow` will calculate a
different hash for the file. As such, we compute our own hash of the metainformation
for the name the serialised file.

In the end the output directory looks like:

```
9442c7eaa81f82f7e9889f6ee8382e8d047df76db2d5f6a6983d1c82399a2698.pickle
5e7e6994db565126a942d66a9435454d8b55cd7d3023dd37f64eca7bbb46df1f.pickle
...
```

### Gotcha 1: Using `listDirContents` defeats caching

Now that we have a directory containing all the metainformation, we want to split
it up and then execute the fetching, converting and warping in parallel for all
the images. My first attempt was

```
meta_dir <- step All <<< scrape -< (script_dir, ())
keys <- listDirContents -< meta_dir
```

but this did not work and even if the keys remained the same, the images would
be refetched. The problem was [`listDirContents`](https://hackage.haskell.org/package/funflow-1.3.2/docs/Control-Funflow-Steps.html#v:listDirContents) does not have the correct caching
behaviour.

`listDirContents` takes a `Content Dir` and returns a `[Content File]` as required
but the `[Content File]` are pointers into places into the `Content Dir`.
This means that if the location of `Content Dir` changes (if there are any changes or new
additions to any files in the directory) then the location of *all* the `[Content File]` will
also be changed. This means the next stage of recompilation will be triggered
despite being unnecessary.

Instead, we have to put each file in the directory into its own store location
so that the its location depends only on itself rather than the other
contents of the directory. I defined the `splitDir` combinator in order to do
this.

```
splitDir :: ArrowFlow eff ex arr => arr (Content Dir) ([Content File])
splitDir = proc dir -> do
  (_, fs) <- listDirContents -< dir
  mapA reifyFile -< fs


-- Put a file, which might be a pointer into a dir, into its own store
-- location.
reifyFile :: ArrowFlow eff ex arr => arr (Content File) (Content File)
reifyFile = proc f -> do
  file <- getFromStore return -< f
  putInStoreAt (\d fn -> copyFile fn d) -< (file, CS.contentFilename f)
```

It could be improved by using a hardlink rather than `copyFile`.

## Step 2: Download, convert and warp

Now we have split the metainformation up into individual components we have
to download, convert and warp the map files.

We define three flows to do this which correspond to three different scripts.

```
fetch = nixScript [relfile|fetch.py|] [[relfile|shell.nix|]]
          (\metadata -> [ outParam, contentParam metadata ])

convertToGif = nixScript [relfile|convert_gif|] []
                (\dir -> [ pathParam (IPItem dir), outParam ])

warp = nixScript [relfile|do_warp|] []
        (\dir -> [ pathParam (IPItem dir), outParam ])
```

Each script takes an individual input file and produces output in a directory
specified by `funflow`.

`fetch.py` is a python script whilst `convert_gif` and `do_warp` are bash
scripts. We can treat them uniformly because of the `nix-shell` shebang.

These steps are all cached by default because they are external processes.

## Step 3: Merge the images together

In order to get a good looking result, we need to group together the processed
images into groups of overlapping images. This time we will use a python script again
invoked in a similar manner. The output is a directory of files which specify the groups,
remember:

1. We have to be careful naming the files so that the names remain stable across compilation.
   In my original program the names were supplied by a counter but now they are the hash of
   the files which were used to create the group.
2. We have to use `splitDir` after creating the output to put each group file into
   it's own store location so the next recompilation step will work.

```
mergeRasters = nixScript [relfile|merge-rasters.py|] [[relfile|merge-rasters.nix|]]
                (\rs -> outParam : map contentParam rs )
```

This command also relies on `merge-rasters.nix` which sets up the correct python
environment to run the script.

### Gotcha 2: `mergeDirs` can also defeat caching

The original implementation of this used `mergeDirs :: arr [Content File] (Content Dir)`
in order to group together the files and pass a single directory to `merge-rasters.py`.

However, this suffers a similar problem to `listDirContents` as `mergeDirs` will
create a new content store entry which contains all the files in the merge directories.
The hash of this store location will then depend on the whole contents of the
directory. In this case these file paths ended up in the output so it would cause
the next steps to recompile even if nothing had changed.

In this case, we would prefer a "logical" group which groups the files together
with a stable filename which wouldn't affect caching.

The workaround for now was to use `splitDir` again to put each processed image into
its own storage path and then pass each filename individually to `merge-rasters.py`
rather than a directory as before.

## Step 4: Making the tiles

Making the tiles is another straightforward step which takes each of the groups
and makes the necessary tiles for that group.

```
makeTiles = nixScript [relfile|make_tiles|] [] (\dir -> [ contentParam dir, outParam, textParam "16" ])
```

### Gotcha 3: `mergeDirs` doesn't merge duplicate files

Once we have made all the tiles we need to merge them all together. This is safe
as we already ensured that they didn't overlap each other. The problem is
that `mergeDirs` will not merge duplicate files. The `make_tiles` step creates
some unnecessary files which we don't need but would cause `mergeDirs` to fail
as they are contained in the output of each directory.

The solution was to write my own version of `mergeDirs` which checks to see
whether a file already exists before trying to merge it. It would be more hygienic
to ensure that the directories I was trying to merge were properly distinct but
this worked well for this use case.

## Step 5: Creating the static site

Our final script is a python script which creates the static site displaying
all the markers and the map tiles. It takes the output of processing all the images and
the metainformation to produce a single html file.

```
leaflet <- step All <<< makeLeaflet -< ( script_dir, (merge_dir, meta_dir))
```

The final step then merges together the static page and all the tiles. This is
a nice bundle we can directly upload and serve our static site.

```
mergeDirs -< [leaflet, tiles]
```

## Putting it all together

The complete flow is shown below:

```
mainFlow :: SimpleFlow () (Content Dir)
mainFlow = proc () -> do
  cwd <- stepIO (const getCurrentDir) -< ()
  script_dir <- copyDirToStore -< (DirectoryContent (cwd </> [reldir|scripts/|]), Nothing)

	# Step 1
  meta_dir <- step All <<< scrape -< (script_dir, ())
  keys <- splitDir -< meta_dir
	# Step 2
  maps <- mapA (fetch) -< [( script_dir, event) | event <- keys]
  mapJpgs <- mapA convertToGif -< [(script_dir, m) | m <- maps]
  merge_dir <- mergeDirs' <<< mapA (step All) <<< mapA warp -< [(script_dir, jpg) | jpg <- mapJpgs ]
  toMerge <- splitDir -< merge_dir
	# Step 3
  vrt_dir <- step All <<< mergeRasters -< (script_dir, toMerge)
  merged_vrts <- splitDir -< vrt_dir
	# Step 4
  tiles <- mergeDirs' <<< mapA (step All) <<< mapA makeTiles -< [(script_dir, vrt) | vrt <- merged_vrts]

	# Step 5
  leaflet <- step All <<< makeLeaflet -< ( script_dir, (merge_dir, meta_dir))

  mergeDirs -< [leaflet, tiles]
```

Once all the kinks are ironed out -- it's quite short but a very powerful specification
which avoids a lot of redundant work being carried out.

### Gotcha 4: `copyDirToStore` can defeat caching

Using `copyDirToStore` seems much more convenient than copying each script into
the store manually but it can again have confusing caching behaviour. The hash
of the store location for `script_dir` depends on the whole `script_dir` directory.
If you change any file in the directory then the hash of it will change. This means
that all steps will recompile if you modify any script!

This is the reason for the `mergeFiles` call in `nixScriptX`. `mergeFiles` will take
the necessary files from `script_dir` and put them into their own store directory. The hash
of this directory will only depend on the files necessary for that step.

## Running the flow

The flow is run with the simple local runner. We pass in a location for the local
store to the runner which is just a local directory in this case. The library
has support for more complicated runners but I haven't explored using those yet.

```
main :: IO ()
main = do
    cwd <- getCurrentDir
    r <- withSimpleLocalRunner (cwd </> [reldir|funflow-example/store|]) $ \run ->
      run (mainFlow >>> storePath) ()
    case r of
      Left err ->
        putStrLn $ "FAILED: " ++ displayException err
      Right out -> do
        putStrLn $ "SUCCESS"
        putStrLn $ toFilePath out
```


### Displaying the outpath

A nice feature of `nix-build` is that it displays the path of the final output
in the nix store once the build has finished. This is possible to replicate using
`funflow` after defining your own combinator. It would be good to put this
in the standard library.

```
storePath :: ArrowFlow eff ex arr => arr (Content Dir) (Path Abs Dir)
storePath = getFromStore return
```

It means that we can run our flow and deploy the site in a single command given
we have a script which performs the deployment given an output path.

Mine looks a bit like:

```
#! /usr/bin/env nix-shell
#! nix-shell -i bash -p awscli
if [[ $# -eq 0 ]] ; then
     echo 'Must pass output directory'
     exit 1
fi
aws s3 sync $1 s3://<bucket-name>
```

Putting them together:

```
cabal new-run | ./upload-s3
```

## Conclusion

Once everything is set up properly, `funflow` is a joy to use. It abstracts
beautifully away from the annoying problems of scheduling and caching leaving the
core logic visible. An unfortunate consequence of the intensional store model
is that debugging why a build step is not being cached can be very time consuming
and fiddly. When I explain the problems I faced, they are obvious but each one
required careful thought and reading the source code to understand the intricacies
of each of the different operations.

It was also very pleasant to combine using `nix` and `funflow` rather than the
suggested `docker` support.

## Related Links

* [Map of orienteering maps](http://mpickering.github.io/maps.html)
* [Source code](https://github.com/mpickering/rg-map)
* [Reddit comments](https://www.reddit.com/r/haskell/comments/9f7kq9/using_funflow_to_cache_a_nix_based_workflow/)
