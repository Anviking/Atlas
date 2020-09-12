# https://docs.github.com/en/developers/webhooks-and-events/configuring-your-server-to-receive-payloads
require 'sinatra'
require 'json'

post '/payload' do
  push = JSON.parse(request.body.read)
  puts "I got some JSON: #{push.inspect}"
end
