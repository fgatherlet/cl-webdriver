#!/usr/bin/env ruby
# coding: utf-8

require 'selenium-webdriver'

Selenium::WebDriver.logger.level = :info

driver = Selenium::WebDriver.for(
  :remote,
  url: "http://127.0.0.1:4444",
  desired_capabilities: :firefox)

#driver = Selenium::WebDriver.for :firefox
driver.navigate.to "https://medley.life"
element = driver.find_element(xpath: "//a[ contains(text(), '頭') ]")
element = driver.find_element(xpath: "//input")
driver.action.
  #key_down(:shift).
  click(element).
  send_keys("abcd").
  #double_click(second_element).
  #key_up(:shift).
  #drag_and_drop(element, third_element).
  perform

#element = driver.find_element(name: 'q')
#element.send_keys "Hello WebDriver!"
#element.submit
#
#puts driver.title

driver.quit
