# BlueRICE

Make input data. This will create `ocean_data.parquet` to be used in RICE50x.
```bash
cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\Blue-SCC\Code\SCC"
conda activate bluerice
python make_input_data.py
```

Navigate to your RICE50x folder then translate the data
```bash
cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\RICE50x"
rscript .\input\translate_rice50x_data.R --clean -n maxiso3
```
Then
```bash
cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\Blue-SCC"
bash Data\SCC\tmp\mc_BASELINE.bat
```

## Monte Carlo
To run the Monte Carlo:

```bash
cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\Blue-SCC\Data\SCC\tmp"
del /Q /S *.*

cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\Blue-SCC\Code\SCC"
conda activate bluerice
python make_mc_bat_files.py Distribution 100
python make_mc_bat_files.py GSA 100
```

Juno: clean
```bash
ssh juno
cd /work/cmcc/fg12520
rm -r bluerice_server/*
```

Then in Git Bash copy file to Zeus
```bash
cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\Blue-SCC\Code\SCC"
rsync -uv context.py zeus:/users_home/seme/fg12520/work/RICE50x/bluerice_server/context.py
rsync -uv utils.py zeus:/users_home/seme/fg12520/work/RICE50x/bluerice_server/utils.py 
rsync -uv mc_scc.py zeus:/users_home/seme/fg12520/work/RICE50x/bluerice_server/mc_scc.py

cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\Blue-SCC\Data\SCC"
rsync -avz tmp/ zeus:/users_home/seme/fg12520/work/RICE50x/bluerice_server/
rsync -avz out/input_distribution.parquet zeus:/users_home/seme/fg12520/work/RICE50x/bluerice_server/input_distribution.parquet
rsync -avz out/input_GSA.parquet zeus:/users_home/seme/fg12520/work/RICE50x/bluerice_server/input_GSA.parquet
```

Connect to Zeus, update RICE50x
```bash
ssc zeus
cd work/RICE50x
git checkout
git pull
```
Clean folders from previous runs
```bash
rm -r results_ocean_distribution/*
rm -r results_ocean_GSA/*
rm -r scc_distribution/*
rm -r scc_GSA/*
```
Run
```bash
conda activate bluerice
bash bluerice_server/distribution/mc_bsubs.sh
bash bluerice_server/GSA/mc_bsubs.sh
```

Count files with
```bash
ls scc_distribution/ -A | wc -l
ls scc_GSA/ -A | wc -l
```

Concatenate results with: 
```bash
conda activate bluerice
python bluerice_server/mc_scc.py scc_distribution
python bluerice_server/mc_scc.py scc_GSA
```

And download. In Git Bash:
```bash
cd "C:\Users\Granella\Dropbox (CMCC)\PhD\Research\Blue-SCC\Data\SCC\out"
rsync -uv zeus:/users_home/seme/fg12520/work/RICE50x/scc_distribution.parquet "scc_distribution.parquet"
rsync -uv zeus:/users_home/seme/fg12520/work/RICE50x/scc_GSA.parquet "scc_GSA.parquet"
```
