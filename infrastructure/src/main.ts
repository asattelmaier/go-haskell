import { App } from 'cdktf';
import { GoGameSocketServer } from './services/go-game-socket-server';

const app = new App();

new GoGameSocketServer(app);

app.synth();
