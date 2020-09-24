const http = require("http");

const server = http.createServer();

server.on("request", async (req, res) => {
  await new Promise((r) => setTimeout(r, 1000));
  let response = '{data: ""}';
  res.writeHead(200, { "content-length": response.length });
  res.write(response);
  res.end();
});

server.listen(1025);
