# MUSC.py

## DESCRIPTION
The `MUSC.py` reads a configuration file, prepares input files, prepares namelists for the model, executes the model and post-processes the output.

## USAGE
```shell
usage: MUSC.py [-h] -config CONFIG -case CASE [-subcase SUBCASE] [--atm-only] [--sfx-only] [--run-only]
```

## OPTIONS

| Option           | Description                    |
|------------------|--------------------------------|
| -h, --help       |show this help message and exit |
| -config CONFIG   |config file                     |
| -case CASE       |case                            |
| -subcase SUBCASE |subcase (default: REF)          |
| --atm-only       |Only install ATM files          |
| --sfx-only       |Only install SURFEX files       |
| --run-only       |Only run the simulation         |

## Examples

```shell
$ MUSC.py -config your_config.py -case CASENAME -subcase SUBCASENAME
```
