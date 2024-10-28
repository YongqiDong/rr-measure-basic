# Reproducibility in Transportation - Measurement tools development

Create a basic set of tools to retrieve paper information and measure its reproducibility.

## How to use this repo

### Create the virtual environment

To create the virtual environment from the `environment.yml` file, run the following commands:

```sh
conda env create -f environment.yml
conda activate RR-measure
```

### About the Elsevier API usage

#### Get your API key

[Elsevier API](https://dev.elsevier.com/)

#### Safely use your API key

Put the API key in a file named `config.yaml` in the parent directory `../config.yaml` of this GitHub repo. The content of the file should be like this:

```yaml
elsevier_api:
    api_key: your_api_key_here
```

## Background information for the project

### IEEE Xplore API to explore the journal

- [IEEE Xplore API](https://developer.ieee.org/)
- [IEEE Xplore API develop registration](https://developer.ieee.org/apps/register)

### Elsevier API to explore the journal

- [Elsevier API](https://dev.elsevier.com/)

### Timeline

- 2024-10-01: Project started
- 2024-11-01: Dataset v0.1 created (expected)

### Literature

- [Try to Start It! The Challenge of Reusing Code in Robotics Research](https://ieeexplore.ieee.org/document/8514000)
- [Get in Researchers; We're Measuring Reproducibility": A Reproducibility Study of Machine Learning Papers in Tier 1 Security Conferences.](https://dl.acm.org/doi/10.1145/3576915.3623130)


### Acknowledgement

It's a project developed under the instruction of materials from [Wu et al. (2024)](https://www.rerite.org/itsc24-rr-tutorial/). Views here are my own and do not represent the views of any organization.

### Reference

- C. Wu, B. Ghosh, Z. Zheng, and I. Martínez, “Reproducibility in transportation research: a hands-on tutorial.” IEEE International Conference on Intelligent Transportation Systems (ITSC), 2024. [Online](https://www.rerite.org/itsc24-rr-tutorial/). Available: https://www.rerite.org/itsc24-rr-tutorial/

### Tips for Git

```
git pull --rebase origin main
git add .
git commit -m "message"
git push origin main
```

### Team

- [Junyi Ji](https://www.jijunyi.com)
- [Ruth Lu](https://github.com/erasedbird)
- [Cathy Wu](https://www.wucathy.com)