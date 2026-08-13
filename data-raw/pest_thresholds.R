pest_code <- c(
  "OLFF", "NOW", "CM", "SWD", "BMSB", "ACP", "PTB", "WHF",
  "CRS", "SJS", "CT", "CLM", "CMB", "AW", "BCW", "CL",
  "CMG", "CPB", "CEW", "CRW", "ECB", "FB", "ICW", "JB",
  "LB", "OM", "SM", "SVB", "SB", "VCW", "WBCW", "WFT",
  "GWSS", "VMB", "LBAM", "EGVM", "CFF", "PL", "BLB", "CSB",
  "SCA", "WSAW", "WCM", "SHW", "PWB", "AM", "BA", "CB",
  "CFB", "CBB", "DPB", "EAB", "FAW", "GPA", "HC", "MBB",
  "OFM", "RB", "SGCA", "TSSM", "WAA", "YSA", "BCA",
  "CA", "FMB", "GA", "GSS", "HWA", "LPT", "OBLR", "PBW",
  "PLH", "SLF", "TMB", "WPB", "BWA", "CNA", "ESB", "GWB",
  "HCT", "LAW", "MRB", "PA", "PBA", "RWA", "SPW", "TA",
  "VPB", "WSB", "ZMA"
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
  "Caribbean Fruit Fly", "Purple Loosestrife", "Bean Leaf Beetle",
  "Common Stalk Borer", "Soybean Aphid", "Wheat Stem Sawfly",
  "Wheat Curl Mite", "Sunflower Headclipping Weevil", "Pale Western Cutworm",
  "Apple Maggot", "Banded Ash Borer", "Cabbage Butterfly",
  "Citrus Flat Mite", "Coffee Bean Borer", "Diamondback Moth",
  "Emerald Ash Borer", "Fall Armyworm", "Green Peach Aphid",
  "Hessian Fly", "Mexican Bean Beetle", "Oriental Fruit Moth",
  "Redbanded Stink Bug", "Sugarcane Aphid",
  "Two-Spotted Spider Mite", "Woolly Apple Aphid", "Yellow Sugarcane Aphid",
  "Brown Citrus Aphid", "Cotton Aphid", "Filbertworm",
  "Green Apple Aphid", "Green Stink Bug", "Hemlock Woolly Adelgid",
  "Lesser Peachtree Borer", "Obliquebanded Leafroller", "Pink Bollworm",
  "Potato Leafhopper", "Spotted Lanternfly", "Tarnished Plant Bug",
  "Western Pine Beetle", "Balsam Woolly Adelgid", "Citrus Nematode",
  "European Chafer", "Grape Root Borer", "Hickory Tussock Moth",
  "Lesser Appleworm", "Mint Root Borer", "Pea Aphid",
  "Pecan Nut Casebearer", "Russian Wheat Aphid", "Sweetpotato Whitefly",
  "Turnip Aphid", "Velvetbean Caterpillar", "Wheat Stem Borer",
  "Zimmerman Pine Moth"
)

lower_thresh <- c(
  60, 55, 50, 50, 57.6, 52, 50, 41,
  52.7, 51, 58.3, 50, 47.3, 48, 50.7, 50,
  43, 50, 54, 52, 50, 50, 48, 50,
  54, 40, 39, 50, 41, 41, 50, 46,
  51, 62, 45, 50, 54, 50, 52, 58.8,
  50, 50, 50, 50, 50, 43.5, 50, 50,
  50, 55.4, 45.1, 54, 58.8, 39.2, 40, 50,
  45, 58, 48.2, 53.6, 41, 55.4, 43.3,
  43.2, 50, 41, 44, 32, 50,
  43, 57, 52.5, 50, 54, 50,
  50, 45, 68, 50, 50, 45,
  50, 41.7, 38, 39.4, 53.6, 42.8,
  59, 51.8, 50.2
)

upper_thresh <- c(
  95, 93.9, 88, 86, 96.4, 91, 88, 93.2,
  86, 90, 95, 95, 93, 90, 86, 90,
  86, 96, 92, 86.7, 86, 90, 90, 90,
  93, 86, 86, 86, 86, 85, 95, 95,
  91, 96, 88, 86, 95, 88.8, 94, 95.1,
  95, 86, 86, 86, 86, 86, 86, 86,
  95, 89.6, 91.4, 97, 95.1, 86, 80, 86,
  90, 95, 95, 86, 95, 88.1, 86,
  86, 86, 85, 87.6, 72, 86,
  85, 91, 86, 95, 93.2, 93.3,
  86.4, 86, 86, 86, 86, 90,
  NA, 82.4, NA, 86, 89.6, 86,
  96.8, 89.6, NA
)

flight_interval_dd <- c(
  720, 1056, 1000, 450, 968, 450, 1060, 1890,
  1145, 1050, 300, 350, 900, 800, 1157, 680,
  1176, 720, 1050, 1300, 1000, 700, 600, 1400,
  680, 1000, 675, 1700, 1400, 1020, 1300, 460,
  1300, 423, 1116, 830, 470, 710, 600, 516,
  180, 100, 180, NA, NA, NA, NA, 581.4,
  380, 562.5, 505.8, NA, 516, 274, 630, 644.6,
  960, 519, 135, 252, 315, 220, 220,
  196, 900, 310, 415, NA, 1220,
  1150, 800, 826, 810, 612, 810,
  1987, 1143, 900, 950, NA, 850,
  1150, 240, 1300, 158, 570.6, 205,
  642.6, 1116, NA
)
pest_thresholds <- data.frame(
  pest_code, pest_name, lower_thresh, upper_thresh,
  flight_interval_dd,
  stringsAsFactors = FALSE
)

usethis::use_data(pest_thresholds, overwrite = TRUE)