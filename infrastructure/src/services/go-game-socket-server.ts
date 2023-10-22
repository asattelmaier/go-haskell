import { Construct } from 'constructs';
import { CloudRunService, CloudRunServiceTemplateSpecContainers } from '@cdktf/provider-google/lib/cloud-run-service';
import { GoogleBackendStack } from '../google-stack/google-backend-stack';
import { CloudRunServiceIamBinding } from '@cdktf/provider-google/lib/cloud-run-service-iam-binding';
import { TerraformVariable } from 'cdktf';

export class GoGameSocketServer extends GoogleBackendStack {
  private static readonly ID = 'go-game-socket-server';
  private static readonly CONTAINER_PORT = 8000;
  private static readonly CONTAINER_CONCURRENCY = 80;
  private static readonly MAX_INSTANCES = '1';
  private static readonly MIN_INSTANCES = '0';
  private readonly gitHash: TerraformVariable = new TerraformVariable(this, 'git-hash', {
    type: 'string',
    description: 'Current Git Hash'
  });

  constructor(scope: Construct) {
    super(scope, GoGameSocketServer.ID);

    const goGameSocketServer = new CloudRunService(this, GoGameSocketServer.ID, {
      location: GoGameSocketServer.DEFAULT_LOCATION,
      name: GoGameSocketServer.ID,
      template: {
        metadata: {
          annotations: {
            'autoscaling.knative.dev/maxScale': GoGameSocketServer.MAX_INSTANCES,
            'autoscaling.knative.dev/minScale': GoGameSocketServer.MIN_INSTANCES,
          }
        },
        spec: {
          containerConcurrency: GoGameSocketServer.CONTAINER_CONCURRENCY,
          containers: [this.getContainerTemplate()],
        },
      },
    });

    // TODO: Make this service only available in a Virtual Private Cloud network
    new CloudRunServiceIamBinding(this, 'allow-public-access', {
      location: GoGameSocketServer.DEFAULT_LOCATION,
      service: goGameSocketServer.name,
      role: 'roles/run.invoker',
      members: ['allUsers'],
    });
  }

  private getContainerTemplate(): CloudRunServiceTemplateSpecContainers {
    const image = `${this.getContainerRegistryRepositoryName()}${GoGameSocketServer.ID}:${this.gitHash.stringValue}`;

    return {
      image,
      ports: [{ containerPort: GoGameSocketServer.CONTAINER_PORT }],
    };
  }
}