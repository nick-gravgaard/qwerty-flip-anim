#!/usr/bin/env bash

# Assumes `elm reactor` is running

rm *.png
timesnap "http://localhost:8000/src/Anims.elm" -S "#svg-main-container" --viewport 1920,1080 --fps 50 --duration 3
convert -delay 1x50 -loop 0 *.png qwerty-flip.gif
cp qwerty-flip.gif $IMAGE_DIR

rm *.png
timesnap "http://localhost:8000/src/Anims.elm" -S "#svg-extras-container" --viewport 1920,1080 --fps 50 --duration 3
convert -delay 1x50 -loop 0 *.png qwerty-flip-extra-moves.gif
cp qwerty-flip-extra-moves.gif $IMAGE_DIR

echo "File copied to $IMAGE_DIR/qwerty-flip.gif"
echo "File copied to $IMAGE_DIR/qwerty-flip-extra-moves.gif"
