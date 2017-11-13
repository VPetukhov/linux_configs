boxquote.el:
	wget http://www.davep.org/emacs/boxquote.el

linum+.el:
	wget https://www.emacswiki.org/emacs/download/linum%2b.el

ssh.el:
	wget https://raw.githubusercontent.com/ieure/ssh-el/master/ssh.el

markdown-mode.el:
	wget https://jblevins.org/projects/markdown-mode/markdown-mode.el

.el.elc:
	emacs -q --no-site-file -batch \
		-eval "(add-to-list 'load-path \".\")" \
		-f batch-byte-compile $*.el
