#!/usr/bin/env python3

import os
import os.path
import subprocess
import sys
import time
import json
import urllib
import urllib.request
import re
import csv
import io


def p(str):
  print(str, flush=True)


def logfile(dir, title):
  return "logs/" + dir + "_" + title + ".log"


def run(dir, title, *args, **kwargs):
  logfilename = logfile(dir, title)
  with open(logfilename, "w") as file:
    result = subprocess.run(*args,
                            stdout=file,
                            stderr=subprocess.STDOUT,
                            **kwargs)
    if result.returncode != 0:
      raise Exception(
          f"Failure running {args} - see {logfilename} for details")
    return result


#def run_and_save(dir, title, *args, **kwargs):
#  with open(logfile(dir, title), "w") as file:
#    result = subprocess.check_output(*args, stderr=subprocess.STDOUT, **kwargs)
#    result = str(result)
#    file.write(result)
#    return result


def TODO(str):
  p("TODO: " + str + "\n\n\n\n")
  sys.exit(-1)


def build(dir):
  p("  Building")
  run(dir, "build", "./build.sh", cwd=dir)


def install(dir):
  p("  Fetching dependencies")
  run(dir, "install", "./install.sh", cwd=dir)


def start_server(dir):
  p("  Starting server")
  file = open(logfile(dir, "server"), "w")
  handle = subprocess.Popen("./exe",
                            cwd=dir,
                            stderr=subprocess.STDOUT,
                            stdout=file)
  time.sleep(2)
  return handle


def start_httpbin(dir):
  p("  Starting httpbin")
  file = open(logfile(dir, "httpbin"), "w")
  handle = subprocess.Popen(
      ["docker", "run", "-p", "80:80", "kennethreitz/httpbin"],
      cwd=dir,
      stderr=subprocess.STDOUT,
      stdout=file)
  time.sleep(2)
  return handle


def stop_handle(name, dir, handle):
  p(f"  Stopping {name}")
  handle.kill()


def measure(dir, url):
  p("  Measuring")
  run(dir, "measure",
      ["wrk", "-c", "100", "-t", "2", "-d", "8", "--timeout", "20", url])


def measure_gently(dir, url):
  p("  Measuring gently")
  run(dir, "measure_gently",
      ["wrk", "-c", "10", "-t", "2", "-d", "10", "--timeout", "20", url])


def report(dir, url):
  results = {}

  with open(logfile(dir, "measure"), "r") as file:
    for line in file.readlines():
      try:
        # general stats
        split = line.strip().split(":")
        if len(split) == 2:
          results[split[0]] = split[1].strip()
        else:
          # percentiles
          split = re.split("\s+", line.strip())
          if len(split) > 1:
            results[split[0]] = [x.strip() for x in split[1:]]
          else:
            # status codes
            split = line.strip().split("]\t")
            if len(split) == 2:
              results[split[0][1:]] = split[1].strip()
            else:
              pass
      except Exception as e:
        print(f"Exception: {e}")
        pass
  errors = results.get("Socket errors", "N/A")

  print(f"    Reqs/s:       {results['Requests/sec']}")
  print(f"    Avg:       {results['Latency'][0]}")
  print(f"    Errors:      {errors}")
  #  print(f"    Fastest:   {results['Fastest'][]}")
  print(f"    Slowest:   {results['Latency'][2]}")


def warmup(dir, url):
  p("  Warming up")
  run(dir, "warmup", ["wrk", "-t", "8", "-c", "50", "-d", "1", url])


def fizzbuzz():
  result = []
  for i in range(1, 101):
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


def test_output(dir, url):
  p("  Testing output")
  response = None
  body = None
  answer = None
  try:
    response = urllib.request.urlopen(url)
    body = response.read()
    answer = json.loads(body)
  except:
    p("Failed to read test output")
    p(f"Response {response}")
    p(f"Body {body}")
    p(f"Answer {answer}")

  return answer == valid_response


def get_url(dir):
  with open(dir + "/port", "r") as myfile:
    lines = myfile.readlines()
    port = "".join(lines)
    port = port.strip()
  return "http://localhost:" + port


def benchmark(dir):
  p("Benchmarking " + dir)
  install(dir)
  build(dir)
  url = get_url(dir)
  fizzbuzz_url = url + "/fizzbuzz"
  fizzboom_url = url + "/fizzboom"
  server_handle = start_server(dir)
  httpbin_handle = start_httpbin(dir)
  try:
    if not test_output(dir, fizzbuzz_url):
      p("  Failed test")
    else:
      warmup(dir, fizzbuzz_url)
      measure(dir, fizzbuzz_url)
      report(dir, fizzbuzz_url)
    measure_gently(dir, fizzboom_url)
    report(dir, fizzboom_url)
  finally:
    stop_handle("server", dir, server_handle)
    stop_handle("httpbin", dir, httpbin_handle)


if len(sys.argv) > 1:
  p(f"Benchmarking just {sys.argv[1:]}")
  for f in sys.argv[1:]:
    benchmark(f)
else:
  p("Starting entire benchmark")
  for f in os.listdir():
    if (os.path.isdir(f) and (not f.startswith("."))
        and (os.path.exists(f + "/build.sh"))):
      benchmark(f)

print("")
