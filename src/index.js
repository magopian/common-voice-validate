import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Main.embed(document.getElementById('root'));
app.ports.resetPlay.subscribe(() => {
  const audio = document.getElementsByTagName("audio")[0];
  audio.currentTime = 0;
  audio.play();
});

registerServiceWorker();
