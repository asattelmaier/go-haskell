# Go Game Socket Server Infrastructure

The infrastructure setup for the Go Game Socket Server using Terraform and Google Cloud
Platform (GCP). It provides the necessary resources and configurations to deploy the server.

## Requirements

- [Terraform](https://developer.hashicorp.com/terraform/tutorials/aws-get-started/install-cli)
- [gcloud](https://cloud.google.com/sdk/docs/install)
- [Node.js](https://nodejs.org/en/download)

## Setup gcloud

Before deploying the infrastructure, make sure you authenticate with GCP and set your project. Run the following
commands in your terminal:

```bash
gcloud auth application-default login
gcloud config set project PROJECT_ID
```

## Build Infrastructure

```bash
yarn build
```

## Deploy Infrastructure

```bash
yarn deploy -var-file=$(pwd)/.tfvars
```
