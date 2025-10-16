# 整柜总成本计算（Shiny App）

一个用 R 语言 Shiny 编写的简易计算器，根据 **采购价区间** 套用 **每吨运费**，计算整柜总价与单吨成本，并提供“运费 +1200”的对比结果。

## 运行方式

### 方式一：RStudio
1. 打开项目，安装依赖：
   ```r
   install.packages(c("shiny","dplyr","scales","tibble"))
   ```
2. 运行 `app.R`（点击 “Run App”）。

### 方式二：命令行
```bash
Rscript -e "install.packages(c('shiny','dplyr','scales','tibble'), repos='https://cloud.r-project.org')"
Rscript -e "shiny::runApp('app.R', host='0.0.0.0', port=8080)"
```

## 参数说明
- **重量（吨）**：整柜总重量。
- **采购价（万元）**：总采购价，以万元计。
- 运费区间及价格在 `app.R` 顶部的 `freight_table` 中可修改。

## 许可
本项目采用 MIT License（见 `LICENSE`）。
