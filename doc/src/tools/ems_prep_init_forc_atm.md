# ems\_lfa2nc.md

## DESCRIPTION
Create `nam1d` needed by `acadfa` to create ARPIFS restart

## USAGE
```shell
ems_prep_init_forc_atm.py [-h] -m MODEL -t TIMESTEP
                          -nc NCFILE -vgrid VERTICAL_GRID 
			  [-o NAMOUT] [--ascii] [--save-init]
                          [--save-forc] [--diags]
```

## OPTIONS

| Option                              | Description                                              |
|----------------------------------|----------------------------------------------------------|
| -h, --help                       |Show this help message and exit                           |
| -m MODEL, --model MODEL          |String representing the model name and version            |
| -t TIMESTEP, --timestep TIMESTEP |Timestep to use for the simulation                        |
| -nc NCFILE                       |NetCDF file describing the case to simulate               |
| -vgrid VERTICAL\_GRID            |Vertical grid description file                            |
| -o NAMOUT, --namout NAMOUT       |Output nam1D filename (default: nam1D)                    |
| --ascii                          |Forcing written in ASCII files instead of in restart file |
| --save-init                      |Saving initial variables in netCDF file (default: False)  |
| --save-forc                      |Saving initial variables in netCDF file (default: False)  |
| --diags                          |Perform some diagnostics (default: False)                 |

## Examples

```shell
$ 
```
