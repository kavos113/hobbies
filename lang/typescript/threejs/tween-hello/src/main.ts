import { Tween, Group, Easing } from "@tweenjs/tween.js";
import { drawRabbit, drawFox } from "./drawing";

startScene("scene1", false);
startScene("scene2", true);

function startScene(id, dynamic) {
  const group = new Group();

  const width = 480;
  const height = 320;

  const scene = document.getElementById(id);

  const canvas = document.createElement("canvas");
  canvas.width = width;
  canvas.height = height;
  scene.appendChild(canvas);

  const context = canvas.getContext("2d");

  const rabbit = { x: width - 50, y: 50 };

  new Tween(rabbit, group)
    .to({ x: width - 50, y: height - 50 }, 3000)
    .easing(Easing.Exponential.InOut)
    .start();

  const fox = { x: 50, y: 50 };

  new Tween(fox, group)
    .to(rabbit, 3000)
    .dynamic(dynamic)
    .duration(3000)
    .easing(Easing.Exponential.InOut)
    .start();

  animate();

  function animate(time) {
    group.update(time);

    // draw background
    context.fillStyle = "rgb(240,250,240)";
    context.fillRect(0, 0, width, height);

    drawRabbit(context, rabbit.x, rabbit.y, "rgb(150,150,150)");
    drawFox(context, fox.x, fox.y, "rgb(200,80,80)");

    requestAnimationFrame(animate);
  }
}
