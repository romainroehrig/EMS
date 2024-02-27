using Documenter


pages = [
   "Introduction" => "introduction.md"
   "Installation" => "installation.md"
   "MUSC config" => "musc_config.md"
]

prettyurls = get(ENV, "CI", nothing) == "true"

format = Documenter.HTML(prettyurls = prettyurls)

makedocs(
    sitename = "EMS",
    format = format,
    pages = pages
)

deploydocs(
    repo = "github.com/ewhelan/EMS.git",
    devbranch = "feature/start_github_pages_doc",
    devurl = "master",
)
