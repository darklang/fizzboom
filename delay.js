const http = require("http");

const server = http.createServer();

const debug = false;

function getRandomInt(max) {
  return Math.floor(Math.random() * Math.floor(max));
}

server.on("request", async (message, res) => {
  let id = getRandomInt(100000);
  let path = message.url.split("/");
  if (path[1] == "delay") {
    let time = parseInt(path[2]);
    if (debug) {
      console.log(`[${id}] Received request to ${message.url}`);
    }
    await new Promise((r) => setTimeout(r, time * 1000));
    if (debug) {
      console.log(`[${id}] Finished delay`);
    }
    let response = '{"data": ""}';
    res.writeHead(200, { "content-length": response.length });
    res.write(response);
    res.end();
    if (debug) {
      console.log(`[${id}] Finished request`);
    }
  } else {
    res.writeHead(404, {});
    res.write("path should be /delay/{number of seconds}");
    res.end();
  }
});

server.listen(1025);
