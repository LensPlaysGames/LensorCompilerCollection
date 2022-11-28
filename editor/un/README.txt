==== INSTALLATION ====

There are a multitude of ways to install this plugin. Common across all of them is this directory, un, *must* be within vim's `rtp`.

Once installed, restart VIM and, upon loading a file with `.un` extension, syntax highlighting should work. To load the proper syntax highlighting manually when the automatic `ftdetect` isn't working, use `:set filetype=un`.

1. Manual External Install (preferred)

Let's say this directory is located at `/path/to/FUNCompiler/editor/un`.

Add the following to your vimrc (see `:help vimrc-intro` if confused).

```vim
filetype off
set rtp+=/path/to/FUNCompiler/editor/un
filetype plugin indent on
syntax on
```

2. Manual Internal Install

Copy all of the subdirectories of this folder into `$VIMRUNTIME/.vim/` directory. If you don't know what $VIMRUNTIME is, use `:help rtp`.

3. TODO: Package Manager(s)

How do VIM package managers work? Vundle? Pathogen? Vim-Plug?


Following *any one* of the above steps will install this plugin.
