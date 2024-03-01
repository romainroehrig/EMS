# ems\_prep\_nam\_sfx.py

## DESCRIPTION
Modify a SURFEX namelist to simulate a case described by a NetCDF file using the DEPHY common format.

## USAGE
```
usage: ems_prep_nam_sfx.py [-h] -nc NCFILE [-lfi | -fa] [-o NAMOUT] namin
```

## OPTIONS

| Option                     | Description                                |
|----------------------------|--------------------------------------------|
| -h, --help                 |Show this help message and exit             |
| namin                      |Input SURFEX namelist                       |
| -nc NCFILE                 |NetCDF file describing the case to simulate |
| -lfi                       |Use LFI format for SURFEX files (default)   |
| -fa                        |Use FA format for SURFEX files              |
| -o NAMOUT, --namout NAMOUT |Name of the output namelist                 |

## Examples

```shell
$ 
```
