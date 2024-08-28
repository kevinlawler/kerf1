
**To create ZFB-full.psv file:**

Quandl sends these marked "csv" but actually containing "psv"
The first line (header) needs to be removed from all but the first file
Then they all need to be joined in as one and placed in `./incoming/ZFB-full.psv`
(Many ways to do this)
One way is to:


1. Unzip the compressed ZFB files
2. Navigate to the folder
3. Move the provided script `process-zfb.sh` to the folder
4. Set the permissions: `chmod +x process-zfb.sh`
5. Run the script `./process-zfb.sh`
6. Place the provided output `ZFB-full.psv` in `./incoming/ZFB-full.psv`


**To write fresh all .table files (~15 minutes):**


    ./kerf parse-save-all.kerf



**To load all tables in Kerf (~10 seconds, assuming they've been saved already):**



    ./kerf load-table-all.kerf



