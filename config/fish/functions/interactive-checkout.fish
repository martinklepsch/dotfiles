# Defined in /var/folders/5q/_0mbk1pj18bcm0c6k56fb2nc0000gn/T//fish.Fx0PPs/interactive-checkout.fish @ line 2
function interactive-checkout --description 'Fuzzy-find and checkout a branch'
	git branch --sort=-committerdate | grep -v '^\*' | fzf | xargs git checkout
end
