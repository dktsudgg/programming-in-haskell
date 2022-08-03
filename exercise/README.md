# exercise

### Install gtk+3 using brew on MacOS
#### This is for a GUI-Based life game 
```shell
# https://github.com/haskell-gi/haskell-gi#mac-osx
brew install gobject-introspection gtk+ gtk+3
```

### gi-gtk example
https://github.com/owickstrom/gi-gtk-declarative/tree/master/examples

### Build project
```shell
stack build
```

### Run using GHCi
```
# Run GHCi
stack ghci

# Load the module you want to use
> :l src/chapters/Chapter9/Lifegame
> startLifeGame

...
```