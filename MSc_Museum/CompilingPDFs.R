# Instructions for downloading PDFs of lectures

# Install decktape following instructions here:
https://github.com/astefanutti/decktape#install

# Run the following code for each lecture
cd decktape-1.0.0
./phantomjs decktape.js slidy -s 1024x720 -p 4000 https://rawgit.com/nhcooper123/TeachingMaterials/master/MSc_Museum/Lectures/intro.html intro.pdf

# -s sets the output viewport size
# -p sets the duration in ms that decktape waits before exporting each slide. 
