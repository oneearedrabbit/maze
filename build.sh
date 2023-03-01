export name=maze8
export shortname="$name"_fmt
cp splash.png pico-label/
cp "$name".p8 maze.p8
p8tool luamin "$name".p8 --keep-all-names
cp "$shortname".p8 pico-label
cd pico-label
python pico_label.py -c "$shortname".p8 -l splash.png             
pico8 -export maze.html "$shortname".p8
mv maze.html index.html
zip maze.zip index.html maze.js
