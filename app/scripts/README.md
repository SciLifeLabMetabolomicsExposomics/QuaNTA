This directory houses the scripts for specific tasks.

### Individual scripts

The name of each script file often reflects what it does. It follows this format.

```
<what it does>-<sample set ID>-<details>[.s(tep )number].<extension>
```

#### `<what it does>`

* `gen` : **Gen**erate file(s) that contain data. The names of generating files are usually the rest of the script file name except the extenstion, i.e. `<sample set ID>-<details>[.s(tep )number]`. The output files are created in the `../../data/intermediate/` or `../../data/processed/` directories.
* `anal` : **Anal**yse data. The `<details>` explains briefly about the models.
* `exp_tsv` : **Exp**ort to **tsv** files, tab-delimited text files. It creates files with the name `<sample set ID>-<details>[.s(tep )number].tsv` often in the `../../outputs/tables/` directory. 

