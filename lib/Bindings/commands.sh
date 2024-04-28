#! /usr/bin/env sh

breakTime() {
    sleep 1200 && watch -n 1200 dunstify -u critical -t 30000 'Break Time'
}

audioMute() {
    pamixer -t
    dunstify -t 750 -h string:x-dunst-stack-tag:'Volume' -h int:value:$(pamixer --get-volume-human) 'Toggle Mute'
}

audioDown() {
    if $(pamixer --get-mute); then
	pamixer -t
    fi

    pamixer -d 10
    dunstify -t 750 -h string:x-dunst-stack-tag:'Volume' -h int:value:$(pamixer --get-volume-human) 'Sound Down'
}

audioUp() {
    if $(pamixer --get-mute); then
	pamixer -t
    fi

    pamixer -i 10
    dunstify -t 750 -h string:x-dunst-stack-tag:'Volume' -h int:value:$(pamixer --get-volume-human) 'Sound Up'
}

brightDown() {
    if [ $(xrandr --query | grep -c 'HDMI-1 connected') -eq 1 ]; then
	xrandr --output HDMI-1 --brightness .8
	dunstify -t 750 -u low -h string:x-dunst-stack-tag:'Brightness' "xrandr brightness low"
    else
	brightnessctl set 5%-

	dunstify -t 750 -u low -h string:x-dunst-stack-tag:'Brightness' -h int:value:$(brightnessctl -m -d amdgpu_bl0 | awk -F, '{print substr($4, 0, length($4)-1)}' | tr -d '%') 'Brightness Down'
    fi
}

brightUp() {
    if [ $(xrandr --query | grep -c 'HDMI-1 connected') -eq 1 ]; then
	xrandr --output HDMI-1 --brightness 1
	dunstify -t 750 -u low -h string:x-dunst-stack-tag:'Brightness' "xrandr brightness high"
    else
	brightnessctl set +5%

	dunstify -t 750 -u low -h string:x-dunst-stack-tag:'Brightness' -h int:value:$(brightnessctl -m -d amdgpu_bl0 | awk -F, '{print substr($4, 0, length($4)-1)}' | tr -d '%') 'Brightness Up'
    fi
}

hdmi() {
    # check whether a laptop screen or external monitor should be on (display)
    if [ $(xrandr --query | grep -c 'HDMI-1 connected') -eq 1 ]; then
	xrandr --output eDP-1 --off --output HDMI-1 --auto
    else
	xrandr --output eDP-1 --auto
    fi
}

lock() {
    if [ $(xrandr --query | grep -c 'HDMI-1 connected') -eq 1 ]; then
	xrandr --output HDMI-1 --brightness .3
	i3lock -ueni ~/Pictures/gem_full.png
	xrandr --output HDMI-1 --brightness 1
    else
	brightnessctl -s set 5 && i3lock -ueni ~/Pictures/gem_full.png; brightnessctl -r
    fi
}

randTerm() {
    rand=$(shuf -i 0-2 -n 1)

    if [ $rand -eq 0 ]
    then
	alacritty
    elif [ $rand -eq 1 ]
    then
	kitty
    else
	wezterm
    fi
}

toggleBar()
{
    # -n checks that the output is not empty
    if [[ -n $(eww active-windows) ]]
    then
	eww close bar
    else
	eww open bar
    fi
}

jobget() {
    text=$(tail -n+$1 ~/Documents/data.txt | head -n1)
    sleep 0.1 && xdotool type "$text"
}

"$@"
