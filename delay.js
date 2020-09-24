const http = require("http");

const server = http.createServer();

server.on("request", async (message, res) => {
  let path = message.url.split("/");
  if (path[1] == "delay") {
    let time = parseInt(path[2]);
    await new Promise((r) => setTimeout(r, time * 1000));
    let response = '{"data": ""}';
    res.writeHead(200, { "content-length": response.length });
    res.write(response);
    res.end();
  } else {
    res.writeHead(404, {});
    res.write("path should be /delay/{number of seconds}");
    res.end();
  }
});

server.listen(1025);
