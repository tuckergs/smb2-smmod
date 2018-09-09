
# smb2-smmod

## Wat?

This program allows you to change the story mode entries and allows you to specify two groups of time sharing level slots.

## Compiling

This is written in Haskell, so you need something like Haskell Platform.

I use a Makefile, so you just need to run
```
make
```

Run
```
make clean
```
to remove the execeutable and compilation files

If you don't have make, you can run
```
ghc Main.hs
```
to compile


## Sample configs

simpleConfig.story.txt gives you vanilla Story Mode. If you run the program using it, it won't change your REL. This is intended to be a template

testConfig.story.txt changes 1-1 to Planets with ten difficulty, it makes the story-exclusive stages (save for World 10) have 45 seconds (i.e. ID <= 68), and makes all the other stages have 120 seconds. This is intended to show you how to specify the time sharing level slots along with the times

simpleWithNewEntriesConfig.story.txt gives you the regular level order with the new story mode entry format. Look at the Melting Pot entry to see how to specify time for the entries. See smb2-relmod for more info about new story mode entries

## Credits

Thanks to miss mecha and various others for figuring out where the story mode entries were and the format of them.
