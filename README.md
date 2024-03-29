# uni-notes
This is where I store all of my knowledge from university. Created with [Obsidian.md](https://obsidian.md/)

# Syncing
Here are the scripts that I use to sync my notes

- Create a `scripts` directory
- Add it to `PATH` via your `.bashrc` or `.zshrc`
	- This is located in your home directory
```bash
export PATH="$PATH:$HOME/scripts"
```
- Copy the scripts from `uni-notes/scripts` to your global scripts directory
```bash
cp /path/to/obsidian/vault/scripts/* ~/scripts
```
- Update the lines with `CHANGE ME!` comments
`push-notes.sh`
```bash
#!/bin/bash
# Absolute path of the Obsidian vault
NOTES_DIR="/home/eilidhm/Desktop/uni/obsidian"  # CHANGE ME!
DEVICE_NAME="laptop"                            # CHANGE ME!
cd $NOTES_DIR
git add -A && git commit -m "Notes from $DEVICE_NAME" && git push
```

`pull-notes.sh`
```bash
#!/bin/bash
# Absolute path of the Obsidian vault
NOTES_DIR="/home/eilidhm/Desktop/uni/obsidian"  # CHANGE ME!
cd $NOTES_DIR
git pull
```

- Make both files executable
```bash
chmod u+x push-notes.sh pull-notes.sh
```
- These can be invoked in a terminal with `push-notes.sh` and `pull-notes.sh`
- You may want to put this in a [cron job](https://archived.forum.manjaro.org/t/how-to-create-a-cron-job-in-manjaro/105)

## Graph settings
- Changes in `.obsidian/graph.json` get ignored, but the file is still in the repo
	- This was done with `git update-index --skip-worktree`
	- Reversible with `git update-index --no-skip-worktree`
- I did this because the graph zoom level (`scale`) and menu state (`open`) change constantly but are not meaningful changes
- The side-effect is that updated filters will not be seen by Git

To update the filters:
```bash
git update-index --no-skip-worktree .obsidian/graph.json
git commit .obsidian/graph.json -m "reason"
git update-index --skip-worktree .obsidian/graph.json
```

See also: https://stackoverflow.com/questions/13630849/git-difference-between-assume-unchanged-and-skip-worktree

# Organisation guidelines
- Organise notes loosely by subject
	- There may be crossover between subjects (E.G CS and Maths, Sociology and Statistics)
- Don't create directories unless there's a sensible grouping. It's fine to have lots of notes in a directory

# Naming conventions
- Top-level directories should be `kebab-case`
- Notes and sub-directories should be `Title Case
- Tags should be `PascalCase` and may be [hierarchal](https://help.obsidian.md/Editing+and+formatting/Tags#Nested+tags)

## Types of files
- [Maps of content](https://ricraftis.au/obsidian/i-have-been-doing-my-obsidian-maps-of-content-mocs-the-wrong-way/) should be called "* Map"
- Overviews for specific courses should be called "{course name} Dashboard"
- Progress/task trackers should be called "* Tracker"