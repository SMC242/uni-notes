#!/bin/bash
# Absolute path of the Obsidian vault
NOTES_DIR="/home/eilidhm/Desktop/uni/obsidian"  # CHANGE ME!
DEVICE_NAME="laptop"  # CHANGE ME!
cd $NOTES_DIR
git add -A && git commit -m "Notes from $DEVICE_NAME" && git push
