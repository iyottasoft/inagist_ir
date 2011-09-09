#!/usr/bin/env python2.6

class BottleNoseAPI:
  def __init__(self):
    print 'AmazonAPI initialized in python code'

  def item_search(self):
    print 'this is working, yay!'
    return 10

def main():
  b = BottleNoseAPI()
  b.item_search()

if __name__ == "__main__":
  main()
