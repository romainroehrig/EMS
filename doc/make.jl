using Documenter


pages = [
   "Introduction" => "introduction.md"
   "Installation" => "installation.md"
   "MUSC configuration" => "musc_config.md"
   "EMS Tools" => [
	"MUSC.py" => "tools/ems_musc.md",
        "ems_list_cases.py" => "tools/ems_list_cases.md",
        "ems_prep_init_forc_atm.py" => "tools/ems_prep_init_forc_atm.md",
        "ems_prep_nam_atm.py" => "tools/ems_prep_nam_atm.md",
        "ems_prep_nam_sfx.py" => "tools/ems_prep_nam_sfx.md",
        "ems_lfa2nc.py" => "tools/ems_lfa2nc.md",
	"ems_convert2p.py" => "tools/ems_convert2p.md",
        "ems_convert2z.py" => "tools/ems_convert2z.md",
        "ems_convertLFA2nc.py" => "tools/ems_convertLFA2nc.md"
	],
   "References" => "references.md"
]

prettyurls = get(ENV, "CI", nothing) == "true"

format = Documenter.HTML(prettyurls = prettyurls)

makedocs(
    sitename = "EMS",
    format = format,
    pages = pages
)

deploydocs(
    repo = "github.com/romainroehrig/EMS.git",
    devbranch = "master",
    devurl = "master",
)
