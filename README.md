# NetWORKHORSE

## Description

NetWORKHORSE offers social network analysis and visualizations that are accessible to all users, regardless of their technical expertise. The ultimate aim is to foster collaboration among researchers and innovators within Apollo Health Innovations.

## Repository Structure

The GitHub repository has the following structure: 

```bash
.
├── Comparison_script.R
├── DCiA.Rproj
├── images
│   ├── Internal_combination.png
│   ├── all3_connections.png
│   ├── co-author.png
│   ├── combination.png
│   ├── grant.png
│   └── knowledge.png
├── README.md
├── rsconnect
│   └── documents
│       └── Shiny_Base.R
│           └── shinyapps.io
│               └── apollo-dashboard
│                   └── DCiA.dcf
├── Shiny_Base.R
└── www
    ├── networkhorse.png
    └── workhorse.png
```

The core application is written in `Shiny_Base.R`. Within this central R file, we incorporate `Comparison_script.R` specifically within the "Compare Networks" tab.

## Access

NetWORKHORSE can be accessed [here](https://apollo-dashboard.shinyapps.io/DCiA/).


## Credits

NetWORKHORSE was developed by Elano Beer, Nufail Billar, Charlot Franssen, Wouter Mulder, India Nunes and Quintine Sol.
