
# smb2-smmod

## Wat?

This program allows you to change the story mode entries 

## READ THIS IF YOU WANT TO CHANGE THE TIME LIMITS FOR STAGES

Note that vanilla SMB2 doesn't allow you to specify times in story mode entries. We have developed two ways to get around this: one, the stupid way, change an instruction to define two groups of levels, where every level in group 1 has one common time, and every level in group 2 has another common time (we call this concept time sharing slots). As you can see, that is incredibly dumb, so the second way is injecting some code into your REL to make the game accept a type of story mode entry that actually specifies time. I'd recommend second way, as first way is stupid. Two things you would need to do: 

1) go to github.com/tuckergs/smb2-relmod and follow the instructions laid out in docs/newSMEntryFormats.txt in order to add the code

2) Use smmod to add the entries. Do not forget to put the line "#entryType New" at the top, or YOUR GAME WILL CRASH when the game tries to load the time limit or level slot or something

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
to remove the executable and compilation files

If you don't have make, you can run
```
ghc Main.hs
```
to compile


## Sample configs

simpleConfig.story.txt gives you vanilla Story Mode. If you run the program using it, it won't change your REL. This is intended to be a template

testConfig.story.txt changes 1-1 to Planets with ten difficulty, it makes the story-exclusive stages (save for World 10) have 45 seconds (i.e. ID <= 68), and makes all the other stages have 120 seconds. This is intended to show you how to specify the time sharing level slots along with the times. Note that this is also the incredibly dumb and 3000 BC way of dealing with time limits. See the new entries config for the 2020 way

simpleWithNewEntriesConfig.story.txt gives you the regular level order with the new story mode entry format. Look at the Melting Pot entry to see how to specify time for the entries. See smb2-relmod for more info about new story mode entries

## Credits

Thanks to miss mecha and various others for figuring out where the story mode entries were and the format of them.
