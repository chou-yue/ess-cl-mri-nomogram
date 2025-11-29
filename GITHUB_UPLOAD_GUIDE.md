# GitHub 上传指南

## 方式一：使用 GitHub Desktop（最简单，推荐新手）

### 1. 安装 GitHub Desktop
- 访问 https://desktop.github.com/
- 下载并安装 GitHub Desktop

### 2. 登录 GitHub 账号
- 打开 GitHub Desktop
- 点击 "Sign in to GitHub.com"
- 输入您的 GitHub 用户名和密码

### 3. 创建新仓库
- 点击 "File" → "New Repository"
- 填写信息：
  - **Name**: `ess-cl-mri-nomogram` （或您想要的名字）
  - **Description**: "MRI-based prediction model for ESS vs CL differentiation"
  - **Local Path**: 选择 `/Users/apple/Desktop/vs code/crm/ess-cl-mri-nomogram-github`
  - 勾选 "Initialize this repository with a README"（可选，因为已有 README）
  - 选择 License: MIT
  - 勾选 ".gitignore": R

### 4. 发布到 GitHub
- 点击 "Publish repository"
- 选择是否设为私有仓库（Private）或公开（Public）
- 点击 "Publish Repository"

✅ 完成！您的项目已上传到 GitHub

---

## 方式二：使用命令行（适合熟悉 Git 的用户）

### 1. 初始化 Git 仓库

打开终端，进入项目目录：

```bash
cd "/Users/apple/Desktop/vs code/crm/ess-cl-mri-nomogram-github"
```

初始化 Git：

```bash
git init
git add .
git commit -m "Initial commit: ESS vs CL prediction model"
```

### 2. 在 GitHub 网站创建新仓库

1. 访问 https://github.com/new
2. 填写仓库信息：
   - **Repository name**: `ess-cl-mri-nomogram`
   - **Description**: "MRI-based prediction model for differentiating endometrial stromal sarcoma from cellular leiomyoma"
   - 选择 **Public** 或 **Private**
   - **不要**勾选 "Initialize this repository with a README"（因为本地已有）
   - License: MIT License
   - .gitignore: R
3. 点击 "Create repository"

### 3. 连接本地仓库到 GitHub

GitHub 会显示一些命令，执行以下命令（替换成您的用户名）：

```bash
git remote add origin https://github.com/YOUR_USERNAME/ess-cl-mri-nomogram.git
git branch -M main
git push -u origin main
```

如果需要输入密码，使用 **Personal Access Token** 而不是密码：
1. 访问 https://github.com/settings/tokens
2. 生成新 token（权限选择 repo）
3. 复制 token 并在命令行中作为密码使用

✅ 完成！

---

## 方式三：使用 VS Code（如果您使用 VS Code）

### 1. 打开项目文件夹
```bash
code "/Users/apple/Desktop/vs code/crm/ess-cl-mri-nomogram-github"
```

### 2. 初始化 Git
- 点击左侧 "Source Control" 图标（或按 `Cmd+Shift+G`）
- 点击 "Initialize Repository"
- 点击 "+" 将所有文件添加到暂存区
- 输入提交信息："Initial commit: ESS vs CL prediction model"
- 点击 ✓ 提交

### 3. 发布到 GitHub
- 点击 "Publish to GitHub"
- 选择仓库名称和公开/私有设置
- 点击 "Publish"

✅ 完成！

---

## 上传后的重要步骤

### 1. 检查仓库
访问您的 GitHub 仓库页面，确认所有文件都已上传

### 2. 启用 GitHub Pages（可选 - 用于 Web Tool）

如果您想让诊断工具在线可用：

1. 进入仓库 Settings → Pages
2. Source 选择 "Deploy from a branch"
3. Branch 选择 "main" 和 "/root"
4. 点击 Save
5. 几分钟后，您的工具将在以下地址可用：
   ```
   https://YOUR_USERNAME.github.io/ess-cl-mri-nomogram/web_tool/diagnosis_tool.html
   ```

### 3. 完善仓库信息

- 添加 Topics（标签）：`mri`, `machine-learning`, `medical-imaging`, `radiology`, `r-statistics`
- 更新仓库描述
- 添加您的联系信息到 README

### 4. 更新 README 中的占位符

编辑 README.md 和 LICENSE 文件中的占位符：
- `[Your Name/Institution]` → 您的姓名或机构
- `[Citation information will be added upon publication]` → 您的引用信息
- `[Your contact information]` → 您的联系方式
- `[Institution names]` → 机构名称
- `[Time period]` → 数据收集时间段

---

## 常见问题

### Q1: 文件太大无法上传？
GitHub 单个文件限制 100MB。如果您的数据文件很大：
- 将数据文件添加到 `.gitignore`
- 在 README 中说明如何获取数据

### Q2: 如何更新已上传的代码？
```bash
cd "/Users/apple/Desktop/vs code/crm/ess-cl-mri-nomogram-github"
git add .
git commit -m "描述您的更改"
git push
```

### Q3: 如何设置私有仓库？
在创建仓库时选择 "Private"。您可以之后在 Settings → Danger Zone 中修改可见性。

### Q4: 如何与他人协作？
在仓库 Settings → Collaborators 中添加协作者的 GitHub 用户名。

---

## 推荐的仓库设置

### Description
```
MRI-based logistic regression model for differentiating endometrial stromal sarcoma from cellular leiomyoma using ADC histogram analysis
```

### Topics (标签)
```
medical-imaging
mri
machine-learning
radiology
r-statistics
prediction-model
gynecologic-oncology
clinical-tool
```

### About Section
- Website: 可以添加您的机构网站或在线工具链接
- 勾选 "Releases" 和 "Packages"

---

## 最佳实践

1. **不要上传敏感数据**：患者数据不应上传到 GitHub
2. **定期提交**：经常保存您的工作进度
3. **写清楚的提交信息**：让其他人理解您的更改
4. **使用分支**：重大更改时创建新分支测试
5. **添加文档**：确保 README 清晰完整

---

需要帮助？访问 https://docs.github.com/cn 查看详细文档。
