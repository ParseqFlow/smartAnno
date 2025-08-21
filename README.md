# smartAnno: Smart Cell Type Annotation Using AI Models

## 简介

smartAnno是一个R包，专门用于单细胞RNA测序数据的智能细胞类型注释。该包通过newapi.nbchat.site平台集成了多个最先进的AI模型，包括GPT-5、DeepSeek-R1、Gemini-2.5-Pro-Thinking、Claude-Sonnet-4等，通过分析marker基因来提供准确的细胞类型注释建议。

## 主要特性

- 🤖 **多AI模型支持**: 通过newapi.nbchat.site平台集成GPT-5、DeepSeek-R1、Gemini-2.5-Pro-Thinking、Claude-Sonnet-4、O3-High等最新AI模型
- 🧬 **智能基因分析**: 自动筛选显著的marker基因进行注释
- 🎯 **上下文感知**: 支持组织背景信息，提高注释准确性
- 📊 **结果比较**: 支持多模型结果对比和共识注释
- 💾 **结果导出**: 支持CSV格式结果导出
- 🔧 **易于集成**: 与Seurat等主流单细胞分析工具无缝集成

## 安装

### 安装依赖包

```r
install.packages(c("httr", "jsonlite", "dplyr"))
```

### 安装smartAnno

```r
# 从本地安装
devtools::install_local("path/to/smartAnno")

# 或者使用R CMD INSTALL
# R CMD INSTALL smartAnno
```

## 快速开始

### 1. 基本用法

```r
library(smartAnno)

# 假设你已经有了Seurat的FindAllMarkers结果
markers <- FindAllMarkers(
  object = seurat_object,
  test.use = "wilcox",
  only.pos = TRUE,
  logfc.threshold = 0.25,
  min.pct = 0.1
)

# 使用smartAnno进行多模型注释
annotations <- annotate_cell_types(
  markers = markers,
  n_genes = 10,
  api_key = "your-newapi-key",
  tissue_context = "PBMC",
  models = c("gpt-5", "deepseek-r1-250528", "gemini-2.5-pro-thinking")
)

# 查看结果
print(annotations)
summary(annotations)
```

### 2. 使用示例数据

```r
# 创建示例数据进行测试
example_markers <- create_example_markers()

# 进行单模型注释测试
result <- annotate_cell_types(
  markers = example_markers,
  n_genes = 5,
  models = "gpt-5",
  api_key = "your-api-key",
  tissue_context = "PBMC"
)

# 或使用多模型进行对比
result_multi <- annotate_cell_types(
  markers = example_markers,
  n_genes = 5,
  models = c("gpt-5", "claude-sonnet-4-20250514"),
  api_key = "your-api-key",
  tissue_context = "PBMC"
)
```

## 详细功能

### 支持的AI模型

通过[newapi.nbchat.site](https://newapi.nbchat.site)平台，smartAnno支持以下最新AI模型：

| 模型名称 | 模型ID | 特点 | 推荐用途 |
|---------|--------|------|----------|
| GPT-5 | `gpt-5` | OpenAI最新旗舰模型，卓越的理解和推理能力 | 复杂细胞类型识别 |
| DeepSeek Reasoning | `deepseek-r1-250528` | 强推理能力，提供详细思考过程 | 疑难细胞类型分析 |
| Gemini 2.5 Pro Thinking | `gemini-2.5-pro-thinking` | Google最新思维链模型，逻辑推理强 | 多基因综合分析 |
| Claude Sonnet 4 | `claude-sonnet-4-20250514` | Anthropic高级模型，生物学知识丰富 | 生物学解释和验证 |
| GPT O3 High | `o3-high` | 高性能通用模型，平衡性能和成本 | 常规注释任务 |
| GPT-4o | `gpt-4o` | 优化版GPT-4，速度快成本低 | 批量处理 |
| Claude 3.5 Sonnet | `claude-3-5-sonnet-20241022` | 平衡版Claude模型 | 日常注释工作 |

### 主要函数

#### `annotate_cell_types()`
主要注释函数，支持以下参数：

- `markers`: FindAllMarkers输出的数据框
- `n_genes`: 每个cluster使用的top基因数量（默认10）
- `p_threshold`: P值阈值（默认0.05）
- `models`: 要使用的AI模型列表
- `api_key`: newapi平台的API密钥
- `tissue_context`: 组织背景信息（可选）
- `species`: 物种信息（默认"human"）

#### `extract_consensus()`
从多个模型结果中提取共识注释：

```r
consensus <- extract_consensus(annotations, method = "majority")
```

#### `export_annotations()`
导出注释结果到CSV文件：

```r
export_annotations(annotations, "results.csv")
```

## API配置

### 获取API密钥

1. 访问 [newapi.nbchat.site](https://newapi.nbchat.site)
2. 注册账号并充值（建议初次充值50-100元用于测试）
3. 在控制台获取API密钥
4. 查看可用模型列表和实时价格

### API使用说明

- **基础URL**: `https://newapi.nbchat.site`
- **支持格式**: 兼容OpenAI、Anthropic、Google等多种API格式
- **计费方式**: 按token使用量计费，不同模型价格不同
- **并发限制**: 支持多模型并行调用，提高处理效率
- **速率限制**: 请遵守平台的API调用频率限制

### 模型价格参考（实时价格请查看官网）

| 模型 | 输入价格 | 输出价格 | 适用场景 |
|------|---------|---------|----------|
| gpt-5 | 较高 | 较高 | 重要项目，高精度需求 |
| deepseek-r1-250528 | 中等 | 中等 | 需要推理过程的分析 |
| gemini-2.5-pro-thinking | 中等 | 中等 | 复杂逻辑推理 |
| claude-sonnet-4-20250514 | 中高 | 中高 | 生物学专业分析 |
| gpt-4o | 较低 | 较低 | 日常批量处理 |
| claude-3-5-sonnet-20241022 | 较低 | 较低 | 成本敏感的项目 |

### 多模型使用建议

- **测试阶段**: 先用1-2个便宜模型测试流程
- **生产环境**: 使用2-3个不同类型模型确保结果可靠性
- **成本控制**: 根据项目预算合理选择模型组合
- **质量保证**: 重要分析建议包含至少一个高端模型

## 使用示例

### 完整工作流程

```r
# 1. 加载包和数据
library(smartAnno)
library(Seurat)

# 2. 运行FindAllMarkers
markers <- FindAllMarkers(
  object = pbmc,
  test.use = "wilcox",
  only.pos = TRUE,
  logfc.threshold = 0.25,
  min.pct = 0.1
)

# 3. 多模型智能注释
annotations <- annotate_cell_types(
  markers = markers,
  n_genes = 10,
  p_threshold = 0.05,
  models = c("gpt-5", "deepseek-r1-250528", "gemini-2.5-pro-thinking", "claude-sonnet-4-20250514"),
  api_key = "your-api-key",
  tissue_context = "外周血单核细胞(PBMC)",
  species = "human"
)

# 4. 查看结果
print(annotations)
summary(annotations)

# 5. 提取共识注释
consensus <- extract_consensus(annotations)
print(consensus)

# 6. 导出结果
export_annotations(annotations, "pbmc_annotations.csv")
```

### 高级用法

```r
# 使用更多基因和更严格的阈值
strict_annotations <- annotate_cell_types(
  markers = markers,
  n_genes = 15,
  p_threshold = 0.01,
  models = "gpt-5",
  api_key = api_key,
  tissue_context = "成人外周血，健康供体，10X Genomics平台"
)

# 多模型对比分析
multi_model_annotations <- annotate_cell_types(
  markers = markers,
  n_genes = 12,
  p_threshold = 0.05,
  models = c("gpt-5", "deepseek-r1-250528", "gemini-2.5-pro-thinking"),
  api_key = api_key,
  tissue_context = "肿瘤组织，免疫微环境分析"
)

# 查看多模型结果差异
model_comparison <- compare_model_results(multi_model_annotations)
print(model_comparison)

# 生成共识注释
consensus_result <- extract_consensus(multi_model_annotations, method = "weighted")
```

## 最佳实践

### 1. 参数选择建议

- **n_genes**: 5-15个，太少信息不足，太多可能引入噪音
- **p_threshold**: 0.01-0.05，根据数据质量调整
- **tissue_context**: 提供详细的组织和实验背景信息

### 2. 多模型选择策略

#### 单模型使用场景
- **快速测试**: 使用`gpt-4o`进行快速原型验证
- **成本控制**: 使用`claude-3-5-sonnet-20241022`进行日常分析
- **高精度需求**: 使用`gpt-5`处理复杂细胞类型

#### 多模型组合策略
- **标准组合**: `gpt-5` + `claude-sonnet-4-20250514` (精度与解释性并重)
- **推理组合**: `deepseek-r1-250528` + `gemini-2.5-pro-thinking` (强推理能力)
- **全面分析**: `gpt-5` + `deepseek-r1-250528` + `claude-sonnet-4-20250514` (三模型共识)
- **成本优化**: `gpt-4o` + `claude-3-5-sonnet-20241022` (性价比最优)

#### 模型特点与适用场景
- **GPT-5**: 最新旗舰模型，适合复杂细胞类型识别和新颖细胞亚群发现
- **DeepSeek-R1**: 提供详细推理过程，适合疑难案例分析和结果解释
- **Gemini-2.5-Pro-Thinking**: 逻辑推理强，适合多基因模式识别
- **Claude-Sonnet-4**: 生物学知识丰富，适合结果验证和生物学解释

### 3. 成本控制与效率优化

#### 成本控制策略
- **分层测试**: 先用`gpt-4o`测试，再用`gpt-5`精细化
- **基因数量优化**: 初步测试用5-8个基因，正式分析用10-15个
- **批量处理**: 合并相似cluster减少API调用次数
- **结果缓存**: 保存中间结果，避免重复调用

#### 多模型效率优化
- **并行调用**: 同时调用多个模型API提高速度
- **智能路由**: 根据cluster复杂度选择合适模型
- **结果复用**: 相似cluster可复用已有注释结果
- **增量更新**: 仅对新增或变化的cluster重新注释

## 多模型处理详解

### 多模型工作流程

```r
# 1. 定义多模型配置
model_config <- list(
  primary = c("gpt-5", "claude-sonnet-4-20250514"),
  secondary = c("deepseek-r1-250528", "gemini-2.5-pro-thinking"),
  fallback = c("gpt-4o", "claude-3-5-sonnet-20241022")
)

# 2. 执行多模型注释
results <- multi_model_annotate(
  markers = markers,
  model_config = model_config,
  api_key = "your-api-key",
  tissue_context = "PBMC",
  parallel = TRUE  # 并行调用提高效率
)

# 3. 结果质量评估
quality_scores <- evaluate_annotation_quality(results)
print(quality_scores)

# 4. 模型一致性分析
consistency_matrix <- analyze_model_consistency(results)
heatmap(consistency_matrix)

# 5. 生成最终共识注释
final_annotations <- generate_consensus(
  results, 
  method = "weighted_voting",
  confidence_threshold = 0.7
)
```

### 结果整合策略

#### 1. 投票机制
- **简单多数**: 超过半数模型同意的结果
- **加权投票**: 根据模型性能分配权重
- **置信度阈值**: 仅接受高置信度的一致结果

#### 2. 冲突解决
- **专家模型仲裁**: 使用GPT-5作为最终决策者
- **生物学验证**: 结合已知marker基因数据库验证
- **人工审核**: 标记需要人工确认的不一致结果

#### 3. 质量控制
- **一致性评分**: 计算模型间注释一致性
- **置信度评估**: 评估每个注释的可信度
- **异常检测**: 识别可能的错误注释

### 高级多模型功能

```r
# 自适应模型选择
adaptive_results <- adaptive_model_selection(
  markers = markers,
  complexity_threshold = 0.8,
  api_key = api_key
)

# 增量式多模型注释
incremental_results <- incremental_annotation(
  new_markers = new_markers,
  previous_results = previous_results,
  models = c("gpt-5", "claude-sonnet-4-20250514")
)

# 模型性能基准测试
benchmark_results <- benchmark_models(
  test_data = validation_markers,
  models = c("gpt-5", "deepseek-r1-250528", "gemini-2.5-pro-thinking"),
  ground_truth = known_annotations
)
```

### 多模型结果可视化

```r
# 模型一致性热图
plot_model_consistency(results)

# 置信度分布图
plot_confidence_distribution(results)

# 注释质量评估图
plot_annotation_quality(results, ground_truth)

# 模型性能比较雷达图
plot_model_performance_radar(benchmark_results)
```

## 故障排除

### 常见问题

1. **API调用失败**
   - 检查网络连接
   - 验证API密钥
   - 确认账户余额

2. **JSON解析错误**
   - 更新到最新版本
   - 检查API响应格式

3. **注释质量不佳**
   - 增加基因数量
   - 提供更详细的组织背景
   - 尝试不同的模型

### 测试API连接

```r
# 运行测试脚本
source("test_api_fixed.R")
test_results <- main_test_fixed()
```

## 贡献

欢迎提交Issue和Pull Request来改进这个包。

## 许可证

MIT License

## 联系方式

如有问题，请通过GitHub Issues联系。

---

**注意**: 使用本包需要有效的newapi平台API密钥。请遵守相关服务条款，合理使用API资源。