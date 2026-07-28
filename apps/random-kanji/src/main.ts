import { setupKanjiRoller } from "./kanji";
import "./style.css";

document.querySelector<HTMLDivElement>("#app")!.innerHTML = `
<section id="center">
  <p id="kanji">RANDOM</p>
  <button id="counter" type="button" class="counter">Throw</button>
</section>`;

setupKanjiRoller(
  document.querySelector<HTMLButtonElement>("#counter")!,
  document.querySelector<HTMLParagraphElement>("#kanji")!,
);
