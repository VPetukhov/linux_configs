default: linum+.elc ssh.elc markdown-mode.elc auto-complete popup.elc sr-speedbar.elc yasnippet.elc color-theme.elc polymode dockerfile-mode.elc

auto-complete:
	git clone https://github.com/auto-complete/auto-complete.git

polymode:
	git clone https://github.com/vitoshka/polymode.git

linum+.el:
	wget https://www.emacswiki.org/emacs/download/linum%2b.el

ssh.el:
	wget https://raw.githubusercontent.com/ieure/ssh-el/master/ssh.el

markdown-mode.el:
	wget https://jblevins.org/projects/markdown-mode/markdown-mode.el

popup.el:
	wget https://raw.githubusercontent.com/auto-complete/popup-el/master/popup.el

sr-speedbar.el:
	wget https://www.emacswiki.org/emacs/download/sr-speedbar.el

yasnippet.el:
	wget https://raw.githubusercontent.com/joaotavora/yasnippet/master/yasnippet.el

color-theme.el:
	wget https://download.savannah.nongnu.org/releases/color-theme/color-theme.el.gz && gunzip color-theme.el.gz

dockerfile-mode.el:
	wget https://raw.githubusercontent.com/spotify/dockerfile-mode/master/dockerfile-mode.el

.el.elc:
	emacs -q --no-site-file -batch \
		-eval "(add-to-list 'load-path \".\")" \
		-f batch-byte-compile $*.el
