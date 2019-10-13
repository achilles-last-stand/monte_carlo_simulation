# 本程序学习自Reproducible Finance with R：Code Flows and Shiny Apps for Portfolio Analysis第十一章。此书第十一章介绍了蒙特卡罗模拟（Monte Carlo Simulation）在R语言中的应用。书中在之前章节构建了投资组合，并根据组合收益率的均值和标准差生成了一系列服从正态分布的收益率，在此基础上了模拟了投资于该组合的1美元在10年内的价值变动。我们从自有数据库中提取了过去十年的标普500指数并计算了日收益率的统计值，在此基础上利用tidyverse、dplyr等程序包，不断对均值标准差取随机正态分布值，模拟了投资于标普500指数的1美元10年内价值变动的可能情况，并运用ggplot2和highcharter包进行了模拟结果的可视化。
# 编程者：王伟光，罗瑞达。
# 注：本程序为学习成果，非原创性程序。
