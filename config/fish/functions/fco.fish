# Defined in /var/folders/gh/2fl6yw1j5_343_yz5068jh2m0000gn/T//fish.if0B0A/fco.fish @ line 2
function fco --description 'Fuzzy-find and checkout a branch'
	git branch --sort=-committerdate --all | grep -v HEAD | string trim | fzf | read -l result; and git checkout "$result"
end
