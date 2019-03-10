# ur-setup script!

# make anything "random" reproducible
set.seed(12345)

# packages
source("./scripts/libraries.R")

# supporting functions
source("./scripts/efa_param.R")
source("./scripts/efa_fun.R")
source("./scripts/org_param.R")
source("./scripts/org_fun.R")
source("./scripts/comp_param.R")
source("./scripts/ms_fun.R")
source("./scripts/plot_fun.R")

# data scripts
source("./scripts/data_s1.R")
source("./scripts/data_s2_ad.R")
source("./scripts/data_s2_79.R")
source("./scripts/data_s3_ad.R")
source("./scripts/data_s3_79.R")
source("./scripts/data_s3_46.R")
source("./scripts/data_s4_ad.R")
source("./scripts/data_s4_46.R")