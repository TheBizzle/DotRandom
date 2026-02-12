# DotRandom

## What is it?

A tool for drafting semi-random Dota teams.  It is intended for use with the Open Hyper AI bots, where the output can be entered into the draft lists in the `general.lua` file.

This tool has an internal banlist, whereby heroes with particularly good bots will not be drafted onto the Radiant team, and heroes with particularly bad bots will not be drafted onto *any* team.

## Why is it?

Open Hyper AI claims to be able to play every hero (it can't—not well, at least) and gives you access to formulas to control what it drafts.  However, the author found that there were about 30-40 heroes that it was never drafting, and another 40+ that it *does* draft but *shouldn't* (because it's so bad at them).  It was also annoying to have it eagerly drafting player picks, or putting the overly powerful heroes on the human team.

## How to use

### Build and run

```sh
stack build && stack exec dotrandom
```

### Usage

The assumption is that you're playing on the Radiant side.  The input format is a series of comma-separated entries in format `<position>: <heroName>`.

  * `position`: A number, 1 through 5
  * `heroName`: The player-facing (non-internal) name of the hero, written with only alphabetical characters.  For example: "Shadow Fiend" becomes "ShadowFiend"; "Nature's Prophet" becomes "NaturesProphet"; "Anti-Mage" becomes "AntiMage"; "Clockwerk" becomes "Clockwerk", while "rattletrap" is an error, since that's only his internal name.

As such, when prompted, enter any heroes reserved for you and your teammates with a format like: `2: ShadowFiend, 5: Hoodwink`.  This will prevent the bots from taking them and from pulling another hero out of the pool to suggest in a slot that will be filled by a human.
