
rstudioapi::jobRunScript("jobs/gbif.R", workingDir=here::here())
rstudioapi::jobRunScript("jobs/col.R", workingDir=here::here())
rstudioapi::jobRunScript("jobs/itis.R", workingDir=here::here())
rstudioapi::jobRunScript("jobs/ott.R", workingDir=here::here())

rstudioapi::jobRunScript("jobs/ncbi.R", workingDir=here::here())


rstudioapi::jobRunScript("jobs/iucn.R", workingDir=here::here())
