-- deps.lua - all-in-one declarative dependency handling
-- https://gist.github.com/martinklepsch/49ed53cc18879fa1055b9bdd77471c95
-- vim: sw=2 tw=2 et
deps = {
  -- probably should not be using master branches here
  ["fennel.lua"] = "https://github.com/bakpakin/Fennel/blob/master/fennel.lua",
  ["fennelview.lua"] = "https://raw.githubusercontent.com/bakpakin/Fennel/master/fennelview.lua",
  ["moses.lua"] = "https://raw.githubusercontent.com/Yonaba/Moses/master/moses.lua"
}

for f, url in pairs(deps) do
  local file_exists, _, code = os.execute("test -f " .. f)
  if not file_exists then
    local cmd = "curl -sL -o " .. f .. " " .. url
    print(cmd)
    os.execute(cmd)
    print("You might want to add the downloaded file to your .gitignore")
  else
    -- print("exists")
  end
end
