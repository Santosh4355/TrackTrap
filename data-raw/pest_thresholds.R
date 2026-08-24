pest_code <- c(
  "OLFF", "NOW", "CM", "SWD", "BMSB", "ACP", "PTB", "WHF",
  "CRS", "SJS", "CT", "CLM", "CMB", "AW", "BCW", "CL",
  "CMG", "CPB", "CEW", "CRW", "ECB", "FB", "ICW", "JB",
  "LB", "OM", "SM", "SVB", "SB", "VCW", "WBCW", "WFT",
  "GWSS", "VMB", "LBAM", "EGVM", "CFF", "BLB", "CSB", "SCA",
  "WSAW", "WCM", "PWB", "AM", "BA", "CB", "CFB", "CBB",
  "DPB", "EAB", "FAW", "GPA", "HC", "MBB", "OFM", "RB",
  "SGCA", "TSSM", "WAA", "YSA", "BCA", "CA", "FMB", "GA",
  "GSS", "HWA", "LPT", "OBLR", "PBW", "PLH", "GWF", "PFF",
  "SLF", "TMB", "WPB", "BWA", "CNA", "ESB", "GWB", "HCT",
  "LAW", "MRB", "PA", "PBA", "RWA", "SPW", "TA", "VPB",
  "WSB", "ZMA"
)

pest_name <- c(
  "Olive Fruit Fly", "Navel Orangeworm", "Codling Moth",
  "Spotted Wing Drosophila", "Brown Marmorated Stink Bug", "Asian Citrus Psyllid",
  "Peach Twig Borer", "Walnut Husk Fly", "California Red Scale",
  "San Jose Scale", "Citrus Thrips", "Citrus Leafminer",
  "Citrus Mealybug", "Alfalfa Weevil", "Black Cutworm",
  "Cabbage Looper", "Cabbage Maggot", "Colorado Potato Beetle",
  "Corn Earworm", "Corn Rootworm", "European Corn Borer",
  "Flea Beetle", "Imported Cabbageworm", "Japanese Beetle",
  "Lygus Bug", "Onion Maggot", "Seedcorn Maggot",
  "Squash Vine Borer", "Stalk Borer", "Variegated Cutworm",
  "Western Bean Cutworm", "Western Flower Thrips", "Glassy-winged Sharpshooter",
  "Vine Mealybug", "Light Brown Apple Moth", "European Grapevine Moth",
  "Caribbean Fruit Fly", "Bean Leaf Beetle", "Common Stalk Borer",
  "Soybean Aphid", "Wheat Stem Sawfly", "Wheat Curl Mite",
  "Pale Western Cutworm", "Apple Maggot", "Banded Ash Borer",
  "Cabbage Butterfly", "Citrus Flat Mite", "Coffee Bean Borer",
  "Diamondback Moth", "Emerald Ash Borer", "Fall Armyworm",
  "Green Peach Aphid", "Hessian Fly", "Mexican Bean Beetle",
  "Oriental Fruit Moth", "Redbanded Stink Bug", "Sugarcane Aphid",
  "Two-Spotted Spider Mite", "Woolly Apple Aphid", "Yellow Sugarcane Aphid",
  "Brown Citrus Aphid", "Cotton Aphid", "Filbertworm",
  "Green Apple Aphid", "Green Stink Bug", "Hemlock Woolly Adelgid",
  "Lesser Peachtree Borer", "Obliquebanded Leafroller", "Pink Bollworm",
  "Potato Leafhopper", "Giant whitefly", "Peach Fruit Fly",
  "Spotted Lanternfly", "Tarnished Plant Bug", "Western Pine Beetle",
  "Balsam Woolly Adelgid", "Citrus Nematode", "European Chafer",
  "Grape Root Borer", "Hickory Tussock Moth", "Lesser Appleworm",
  "Mint Root Borer", "Pea Aphid", "Pecan Nut Casebearer",
  "Russian Wheat Aphid", "Sweetpotato Whitefly", "Turnip Aphid",
  "Velvetbean Caterpillar", "Wheat Stem Borer", "Zimmerman Pine Moth"
)

lower_thresh <- c(
  60, 55, 50, 50, 57.6, 52, 50, 41,
  53, 51, 58.3, 54, 47.3, 48, 50.7, 50,
  43, 50, 54, 52, 50, 50, 48, 50,
  54, 40, 39, 50, 41, 45, 50, 50,
  54, 58, 45, 50, 50, 46, 41, 50,
  50, 50, 50, 44.1, 50, 50, 46.4, 55.4,
  45.1, 54, 56.3, 39.2, 40, 50, 45, 58,
  48.2, 53.1, 41, 55.4, 43.3, 43.2, 50, 41,
  54, 32, 50, 43, 57, 52.5, 49.8, 54.7,
  50, 54, 50, 32, 68, 50, 50, 50,
  45, 50, 41.7, 38, 39.4, 53.6, 42.8, 58,
  50, 50
)

upper_thresh <- c(
  95, 93.9, 88, 86, 96.4, 90, 88, 93.2,
  86, 90, 95, 91, 92.5, 90, 86, 90,
  86, 96, 92, 86, 86, NA, 90, 88,
  93, 86, 86, 86, 86, 80, 95, 95,
  100, 90.5, 88, 86, 95, NA, NA, 95,
  86, 86, 86, 86, NA, NA, 95, 89.6,
  91.4, 97, 93.4, 86, 80, 86, 90, 95,
  95, 100.4, 86, 95, 88.1, 86, 86, 85,
  95, 72, NA, 85, 91, 86, 86, NA,
  95, 93.2, 100, 86.4, 86, 86, NA, NA,
  90, NA, 82.4, NA, 86.5, 89.6, 86, 95,
  95, NA
)

flight_interval_dd <- c(
  720, 1056, 1000, 450, 968, 380, 1060, NA,
  1100, 1050, 310, 322, 979, 800, 1157, 791.5,
  1176, 720, 873, 684, 1000, 650, 600, 1400,
  680, 1000, 675, 1700, 1400, 1260, 1320, 460,
  1160, 1032, 1116, 830, 1357, 674, 1400, 180,
  NA, 180, NA, NA, NA, 550, 934, 562.5,
  505.8, NA, 734.89, 274, 630, 644.6, 960, 519,
  135, 195.5, 315, 220, 220, 196, 900, 310,
  680, NA, NA, 1150, 800, 826, 435, 818,
  NA, 612, 1008, 1104, 1143, 900, 950, NA,
  850, 1150, 240, 1300, 158, 570.6, 205, 700,
  970, 1700
)

pest_thresholds <- data.frame(
  pest_code, pest_name, lower_thresh, upper_thresh,
  flight_interval_dd,
  stringsAsFactors = FALSE
)

usethis::use_data(pest_thresholds, overwrite = TRUE)