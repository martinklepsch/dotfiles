#!/bin/bash

INTERFACE="Wi-Fi"
MANUAL_IP="172.20.10.3"
SUBNET="255.255.255.0"
ROUTER="172.20.10.1"

# Get current IP
current_ip=$(ipconfig getifaddr $(networksetup -listallhardwareports | awk "/$INTERFACE/,/Ethernet/"'{ if ($1 == "Device:") print $2 }'))

# Function to show current status
show_status() {
    echo "🌐 Network Status:"
    if [ -z "$current_ip" ]; then
        echo "No IP assigned or interface down"
    else
        echo "Current IP: $current_ip"
    fi
}

# Toggle between DHCP and Manual
if [ "$current_ip" = "$MANUAL_IP" ]; then
    echo "📡 Switching to DHCP..."
    networksetup -setdhcp "$INTERFACE"
    sleep 2
    current_ip=$(ipconfig getifaddr $(networksetup -listallhardwareports | awk "/$INTERFACE/,/Ethernet/"'{ if ($1 == "Device:") print $2 }'))
    echo "✅ Now using DHCP"
else
    echo "🔧 Setting manual IP..."
    networksetup -setmanual "$INTERFACE" "$MANUAL_IP" "$SUBNET" "$ROUTER"
    echo "✅ Set to manual IP: $MANUAL_IP"
fi

show_status
