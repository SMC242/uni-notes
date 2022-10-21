# uni-notes
This is where I store all of my knowledge from university. Created with [Obsidian.md](https://obsidian.md/)

# Syncing
Here are the scripts that I use to sync my notes

- Create a `scripts` directory
- Add it to `PATH` via your `.bashrc`/`.zshrc`
```bash
export PATH="$PATH:$HOME/scripts"
```
- Add the following scripts:
`push-notes.sh`
```bash
#!/bin/bash
# Absolute path of the Obsidian vault
NOTES_DIR="/home/eilidhm/Desktop/uni/obsidian"  # CHANGE ME!
DEVICE_NAME="laptop"  # CHANGE ME!
cd $NOTES_DIR
git commit -a -m "Notes from $DEVICE_NAME" && git push
```

`pull-notes.sh`
```bash
#!/bin/bash
# Absolute path of the Obsidian vault
$NOTES_DIR="/home/eilidhm/Desktop/uni/obsidian"  # CHANGE ME!
cd $NOTES_DIR
git pull
```

- These can be invoked in a terminal with `push-notes.sh` and `pull-notes.sh`
- You may want to put this in a [cron job](https://archived.forum.manjaro.org/t/how-to-create-a-cron-job-in-manjaro/105)