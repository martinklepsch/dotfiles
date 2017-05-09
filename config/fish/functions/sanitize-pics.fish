function sanitize-pics
	exiftool '-filename<${datetimeoriginal}_${model;}.%e' -d "%Y-%m-%d_%H-%M-%S%%-c" $argv[1]

  # normalize file extensions, prompt if conflict
  for f in *.JPG;  mv $f (basename $f ".JPG").jpg; end
  for f in *.JPEG; mv $f (basename $f ".JPEG").jpg; end
  for f in *.jpeg; mv $f (basename $f ".jpeg").jpg; end
  for f in *.MOV;  mv $f (basename $f ".MOV").mov; end
end
