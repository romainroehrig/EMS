# ems\_prep\_nam\_atm.py

## DESCRIPTION
Modify an ARPIFS namelist to simulate a case described by a NetCDF file using the DEPHY common format.

## USAGE
```
ems_prep_nam_atm.py [-h] -m MODEL -t TIMESTEP
                         [--surfex | --nosurfex]
                         -nc NCFILE [-o NAMOUT]
                        namin
```

## OPTIONS

| Option                           | Description                                    |
|----------------------------------|------------------------------------------------|
| -h, --help                       |show this help message and exit                 |
| namin                            |Input ARPIFS namelist (required)                |
| -m MODEL, --model MODEL          |String representing the model name and version  |
| -t TIMESTEP, --timestep TIMESTEP |Timestep to use for the simulation              |
| --surfex                         |Enable surfex usage (default)                   |
| --nosurfex                       |Disable surfex usage                            |
| -nc NCFILE                       |NetCDF file describing the case to simulate     |
| -o NAMOUT, --namout NAMOUT       |output namelist                                 |

## Examples

```shell
$ 
```

