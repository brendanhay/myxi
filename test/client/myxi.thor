#!/usr/bin/env ruby

require "bundler"

Bundler.setup

require "amqp"

class Myxi < Thor

  desc "publish", "connect, publish, and disconnect"
  method_options :messages => 1
  def publish
    EventMachine.run do
      connection = AMQP.connect(connections.first)
      channel = AMQP::Channel.new(connection)

      channel.on_error do |ch, channel_close|
        puts "Channel-level error: #{channel_close.reply_text}, shutting down..."
        connection.close { EventMachine.stop }
      end

      exchange = channel.topic("a.topic", :durable => true, :auto_delete =>  true)
      queue = channel.queue("a.queue", :auto_delete => true).bind(exchange, :routing_key => "events.#")

      published = options[:messages]

      EM.add_periodic_timer(0.01) do
        exchange.publish('hello world', :routing_key => "events.hits.homepage", :persistent => true, :nowait => false) do
          puts "Publishing..."
        end
        if (published -= 1) == 0
          puts "About to disconnect..."
          connection.close { EventMachine.stop }
        end
      end
    end
  end

  private

  def connections
    @connections ||= config[:connections]
  end

  def config
    @config ||= File.open("config.yml", "r") do |file|
      YAML.load(file)
    end
  end

end # Myxi
