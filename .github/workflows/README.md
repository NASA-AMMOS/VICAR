# VICAR GitHub Actions CI/CD

Automated builds for VICAR using GitHub Actions.

## Quick Start

### Zero Setup Required! ✨

GitHub Actions is automatically enabled. Just push to trigger a build:

```bash
git push origin main
```

Or manually trigger a build:
1. Go to **Actions** tab in GitHub
2. Select **"Build VICAR"** workflow
3. Click **"Run workflow"**
4. Choose branch and options
5. Click **"Run workflow"** button

## What Gets Built

The GitHub Actions workflow builds VICAR including:
- **Core VICAR** (RTL, TAE, subsystems)
- **VISOR** (Mars mission programs - 109 applications)
- **Java components** (JavaVicarIO, JadeDisplay, JADIS, SITH, JPIG)
- **P1, P2, P3 programs** (image processing applications)
- **MARS subsystem** (surface mission tools)

## Workflows

### `build-vicar.yml` (Main)

Primary workflow using Rocky Linux 8 container.

**Container:** `rockylinux:8`
- 100% RHEL 8 binary compatible
- Free and open source (no subscription required)
- Full package repository access
- Compatible with Oracle Linux 8 and RHEL 8 (matches internal JPL build environment)

**Triggers:**
- Push to `main`, `master`, `develop` branches
- Push to `feature/*` and `release/*` branches
- Pull requests to main branches
- Weekly schedule (Sundays at midnight UTC)
- Manual trigger with options

**Build Time:** ~1-2 hours

**Features:**
- Downloads open source externals from GitHub releases
- Full VICAR build (TAE → Core → Java → Applications)
- Comprehensive error checking
- Stores build logs and binaries as artifacts
- Optional test job (disabled by default)
- Auto-creates draft releases for version tags

**Manual Options:**
- `external_version`: Externals version (default: 5.0)
- `enable_debug`: Enable debug output (default: false)

### Build Environment

The workflow uses Rocky Linux 8, which is:
- ✅ 100% binary compatible with RHEL 8
- ✅ Free and open source (community-driven)
- ✅ No subscription or registration required
- ✅ Full access to all RHEL 8 packages
- ✅ Compatible with Oracle Linux 8 (internal JPL builds)
- ✅ Production-ready and enterprise-grade
- ✅ Backed by CIQ and community support

**Repositories enabled:**
- BaseOS - Core packages
- AppStream - Application streams
- PowerTools - Development packages
- EPEL - Extra Packages for Enterprise Linux (for libbsd and other extras)

## Build Process

```
┌─────────────────────────────────────────────────┐
│  1. Install Dependencies                        │
│     (compilers, libraries, tools)               │
├─────────────────────────────────────────────────┤
│  2. Setup Environment                           │
│     (symlinks, paths, Java)                     │
├─────────────────────────────────────────────────┤
│  3. Checkout VICAR + Git LFS                    │
├─────────────────────────────────────────────────┤
│  4. Download Open Source Externals              │
│     (from GitHub releases)                      │
├─────────────────────────────────────────────────┤
│  5. Build VICAR                                 │
│     ├─ Prep environment                         │
│     ├─ Fetch and build TAE                      │
│     ├─ Build VICAR Part 1 (core)                │
│     ├─ Build Java components                    │
│     └─ Build VICAR Part 2 (applications)        │
├─────────────────────────────────────────────────┤
│  6. Check Build Logs                            │
│     (scan for errors)                           │
├─────────────────────────────────────────────────┤
│  7. Upload Artifacts                            │
│     (logs and binaries)                         │
└─────────────────────────────────────────────────┘
```

## Monitoring Builds

### View Build Status

1. Go to **Actions** tab in repository
2. See all workflow runs with status (✅ success, ❌ failed, 🟡 in progress)
3. Click on a run to see detailed logs

### Real-time Logs

1. Click on running workflow
2. Click on **"build-vicar"** job
3. Expand steps to see output
4. Logs update in real-time

### Using GitHub CLI

```bash
# Install GitHub CLI
brew install gh  # macOS
# or: sudo apt install gh  # Linux

# View recent runs
gh run list --workflow=build-vicar.yml

# Watch build in real-time
gh run watch

# View logs
gh run view <run-id> --log
```

## Download Build Artifacts

After build completes:

1. Go to workflow run page
2. Scroll to **"Artifacts"** section
3. Download:
   - **`vicar-build-logs`** - All build logs and summary (30 days retention)
   - **`vicar-binaries`** - Compiled binaries and libraries (7 days retention)

Or via CLI:
```bash
gh run download <run-id>
```

## Customization

### Change Externals Version

Edit `.github/workflows/build-vicar.yml`:

```yaml
env:
  EXTERNAL_VERSION: "5.1"  # Update version here
```

Or when manually running, enter version in the input field.

### Add Camera Models

Insert this step before the build step:

```yaml
- name: Download Mars 2020 calibration
  run: |
    cd /usr/local/vicar
    curl -L -o m20_part1.tar.gzaa \
      https://github.com/NASA-AMMOS/VICAR/releases/download/5.0/visor_calibration_20230608_m20.tar.gzaa
    curl -L -o m20_part2.tar.gzab \
      https://github.com/NASA-AMMOS/VICAR/releases/download/5.0/visor_calibration_20230608_m20.tar.gzab
    cat m20_part*.tar.gza* > m20_cal.tar.gz
    tar -zxf m20_cal.tar.gz
```

**Available Calibration:**
- Mars 2020 (M20) - Perseverance and Ingenuity
- Mars Science Laboratory (MSL) - Curiosity
- InSight (NSYT)
- Mars Exploration Rover (MER)
- Phoenix (PHX)
- MSAM

### Enable Testing

Change test job condition in `build-vicar.yml`:

```yaml
test-vicar:
  needs: build-vicar
  if: true  # Changed from false
```

### Add Build Timeout

```yaml
- name: Build VICAR
  timeout-minutes: 240  # 4 hours instead of default 3
  run: |
    tcsh build_open_vicar.csh
```

### Add Slack Notifications

```yaml
- name: Notify Slack on failure
  if: failure()
  uses: slackapi/slack-github-action@v1
  with:
    payload: |
      {
        "text": "VICAR build failed: ${{ github.server_url }}/${{ github.repository }}/actions/runs/${{ github.run_id }}"
      }
  env:
    SLACK_WEBHOOK_URL: ${{ secrets.SLACK_WEBHOOK }}
```

### Use Caching

Cache externals to speed up builds:

```yaml
- name: Cache externals
  uses: actions/cache@v4
  id: cache-externals
  with:
    path: /usr/local/vicar/external
    key: vicar-externals-${{ env.EXTERNAL_VERSION }}

- name: Download externals
  if: steps.cache-externals.outputs.cache-hit != 'true'
  run: |
    # Download and extract...
```

## Advanced Features

### Matrix Builds

Build multiple configurations in parallel:

```yaml
jobs:
  build-vicar:
    strategy:
      matrix:
        project: [PROJ_OS, PROJ_MSL, PROJ_M2020]
    name: Build VICAR (${{ matrix.project }})
    steps:
      - name: Build
        run: |
          util/process_project_file.csh vicset1.source ${{ matrix.project }} > vicset1.csh
          tcsh build_open_vicar.csh
```

### Auto-Release on Tags

The workflow automatically creates draft releases for version tags:

```bash
git tag v5.1.0
git push origin v5.1.0
```

This will:
1. Trigger the build
2. Create a draft release with binaries
3. Generate release notes

### Status Badge

Add to your README.md:

```markdown
[![Build VICAR](https://github.com/NASA-AMMOS/VICAR/actions/workflows/build-vicar.yml/badge.svg)](https://github.com/NASA-AMMOS/VICAR/actions/workflows/build-vicar.yml)
```

### Branch Protection

Require builds to pass before merging:

1. Go to **Settings** → **Branches**
2. Add branch protection rule for `main`
3. Enable **"Require status checks to pass"**
4. Select **"build-vicar"**

## Troubleshooting

### Build Failed?

1. **Check build logs** in Actions tab → Click workflow → Click job
2. **Common issues:**
   - External download failed → Verify GitHub release exists
   - Out of disk space → Add cleanup step (see Advanced Features)
   - Build timeout → Increase `timeout-minutes`
   - Package install failed → Check Oracle Linux repos

### Download Logs

```bash
# Using GitHub CLI
gh run download <run-id> --name vicar-build-logs

# Or from GitHub UI
Actions → Click workflow → Artifacts section → Download
```

### Enable Debug Logging

Set repository secret:
- Name: `ACTIONS_STEP_DEBUG`
- Value: `true`

Or manually run workflow with `enable_debug: true`

### Container Issues?

Try the Ubuntu native workflow (`build-vicar-ubuntu.yml`):
- Uses Ubuntu 22.04 instead of Oracle Linux
- No container overhead
- May have different package compatibility

## Cost

### Public Repositories
- **Free unlimited minutes** ✅
- No cost for builds

### Private Repositories
- **2,000 minutes/month free**
- **$0.008 per minute** after free tier
- ~120 min/build = ~$0.96 per build after free tier

### Storage
- **500MB per artifact**
- **Retention:** Configurable (30 days for logs, 7 days for binaries)
- Included in GitHub plan

## Performance

- **Build Time:** 90-120 minutes (typical)
- **Startup Time:** 4-6 minutes
- **Total Time:** ~2 hours
- **Parallel Jobs:** Up to 20 concurrent (free tier)

## Differences from Internal JPL Build

| Aspect | Internal (JPL) | GitHub Actions |
|--------|----------------|----------------|
| Base Image | Oracle Linux 8 / RHEL 8 | Rocky Linux 8 (RHEL 8 compatible) |
| Externals | Proprietary + Open source | Open source only |
| Build Config | PROJ_ALL (all subsystems) | PROJ_OS (open source) |
| Build Script | `build_open_vicar.csh` | Same |
| Infrastructure | Jenkins + Artifactory | GitHub Actions |
| Access | JPL internal | Public |

## Resources

- [VICAR Documentation](https://nasa-ammos.github.io/VICAR-DOCS/)
- [VICAR Repository](https://github.com/NASA-AMMOS/VICAR)
- [VICAR Releases](https://github.com/NASA-AMMOS/VICAR/releases)
- [GitHub Actions Docs](https://docs.github.com/en/actions)
- [Workflow Syntax](https://docs.github.com/en/actions/reference/workflow-syntax-for-github-actions)

## Support

- **Google Group**: https://groups.google.com/forum/#!forum/vicar-open-source/
- **OpenPlanetary Slack**: #vicar channel
- **Email**: vicar_help@jpl.nasa.gov
- **Issues**: https://github.com/NASA-AMMOS/VICAR/issues

---

**Questions?** Check the comprehensive guide in [README.md](README.md) or file an issue!
