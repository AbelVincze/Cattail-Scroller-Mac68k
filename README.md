# Cattail Scroller M68k

Cattail scroller is a small assembly demo for Mac 68k.
Here is a short video of the result:
https://youtu.be/Iaxj8MvSxHE

## Building

The sources are Fantasm 5.3 assembly. They build on the host with `fantasm` from
Mac68k_devtools:

```bash
fantasm build      # -> build/cattail (+ build/cattail.map)
fantasm run        # build, copy into the maccemu68k LC 475 and launch
```

* `cattail.fproj` is the project, with the settings of the original Anvil project.
* `cattail.rsrc.json` lists the 8 MPackerX-packed DATA resources, whose data is in
  `assets/rsrc/`. These are the files as stored in the resource fork: some have other
  names than in `assets/`, and odd-sized ones have a pad byte.
* `DEBUG_MODE` in `main.def` is 0. `fantasm build --set DEBUG_MODE=1` gives the debug
  build: the demo screen at the top left, a raster-time bar, and debug text that needs a
  screen at least 1024 pixels wide.


The demo is 100 % human coded, only building it on the current macOS was made possible by Claude.
