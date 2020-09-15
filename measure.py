#!/usr/bin/env python3

import os
import os.path
import subprocess
import sys
import time
import json
import urllib
import urllib.request


def p(str):
  print(str, flush=True)


def TODO(str):
  p("TODO: " + str + "\n\n\n\n")
  sys.exit(-1)


def build(dir):
  p("    Building")
  subprocess.run("./build.sh", cwd=dir)


def install(dir):
  p("    Fetching dependencies")
  subprocess.run("./install.sh", cwd=dir)


def start_server(dir):
  p("    Starting server")
  handle = subprocess.Popen("./exe", cwd=dir)
  time.sleep(2)
  return handle


def stop_server(dir, handle):
  p("    Stopping server")
  handle.kill()


def measure(dir, port):
  p("    Measuring")
  TODO("measure")


def warmup(dir, port):
  p("    Warming up")
  TODO("warm up")


def fizzbuzz():
  result = []
  for i in range(1, 100):
    if i % 15 == 0:
      result.append("fizzbuzz")
    elif i % 5 == 0:
      result.append("buzz")
    elif i % 3 == 0:
      result.append("fizz")
    else:
      result.append(str(i))
  return result


valid_response = fizzbuzz()


def test_output(dir, port):
  p("    Testing output")
  response = urllib.request.urlopen("http://localhost:" + port)
  body = response.read()
  answer = json.loads(body)
  if answer != valid_response:
    raise "Bad output"


def get_port(dir):
  with open(dir + "/port", "r") as myfile:
    lines = myfile.readlines()
    port = "".join(lines)
    port = port.strip()
  return port


def benchmark(dir):
  p("\n\n\n\n  Benchmarking " + dir)
  install(dir)
  build(dir)
  port = get_port(dir)
  handle = start_server(dir)
  try:
    if not test_output(dir, port):
      p("    Failed test")
    else:
      results = warmup(dir, port)
      results = measure(dir, port)
  finally:
    #  time.sleep(10000)
    stop_server(dir, handle)


p("Starting")
for f in os.listdir():
  if (os.path.isdir(f) and (not f.startswith("."))
      and (os.path.exists(f + "/build.sh"))):
    benchmark(f)
