#!/bin/bash

# Make symlinks from this directory to your home directory
# Do not put a dot in front of your files as this does it automagically

directory=~/dotfiles
output=testdir
files="vimrc gitconfig"
folders="vim"

echo "Symlinking files to home directory"
for file in $files; do
    echo "Made symlink for $file in home directory"
    ln -s $directory/$file $output/.$file
done

echo "Moving folders to home directory"
for folder in $folders; do
    echo "Made symlink for $folder in home directory"
    ln -s $directory/$folder $output/.$folder
done
