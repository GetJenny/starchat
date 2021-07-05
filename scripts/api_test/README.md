## Scripts calling StarChat's APIs

Here, you can find scripts calling StarChat's APIs. There are both general purpose scripts and those you need to launch your (test) version.

To launch it:

```sh
# start elasticsearch
cd docker-starchat
docker-compose up elasticsearch
```

and then configure elasticsearch. To do that:

```sh
scripts/api_test
./postHealthCheck.sh
./postSystemIndexManagementCreate.sh
./postLanguageIndexManagement.sh
./postIndexManagementCreate.sh
./insertUser.sh
./loadDecisionTableFile.sh
./postAnalyzer.sh
```

In case you want to clean elasticsearch, in `docker-starchat` just:

```sh
rm -rf elasticsearch/data/nodes/*
```

```
./deleteAllDT.sh index_getjenny_english_0 true
./deleteIndexManagement.sh
./postIndexManagementCreate.sh
./loadDecisionTableFile.sh index_getjenny_english_0 ~/Downloads/DT\ Test\ plugin\ -\ Sheet1.csv
./postAnalyzer.sh
```
