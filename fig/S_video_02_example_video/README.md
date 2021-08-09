Images from `15e612cd.2020-07-08_22-05-10.jpg` to `15e612cd.2020-07-15_11-54-20.jpg`.
Video made with:

```
j=0; for i in $(ls 15e*); do mv $i video_$(printf "%04d\n" $j).jpg; j=$((j+1))  ;done 
ffmpeg -framerate 10 -i video_%04d.jpg -c:v libx264 -pix_fmt yuv420p video.mp4
```
