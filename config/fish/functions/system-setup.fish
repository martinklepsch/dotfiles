function system-setup
  echo "Setting screenshot location to folder in iCloud Drive..."
  defaults write com.apple.screencapture location ~/Library/Mobile\ Documents/com~apple~CloudDocs/Screenshots

  echo "Downloading and installing z from https://github.com/sjl/z-fish ..."
  curl https://raw.githubusercontent.com/sjl/z-fish/de2dec23e46ae8620bf2b2d90eea924021107407/z.fish > ~/.config/fish/functions/z.fish
end
