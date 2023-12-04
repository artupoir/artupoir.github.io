const wrapper = document.querySelector(".wrapper");
const question = document.querySelector(".question");
const gif = document.querySelector(".gif");
const noBtn = document.querySelector(".no-btn");
const yesBtn= document.querySelector(".yes-btn");

yesBtn.addEventListener( "click" , ( ) => {
    question.innerHTML = "Aaaaa, Aku Juga Sayang Banget Sama Kamu";
    gif.src = "https://media.tenor.com/-r71xTSUvD0AAAAi/peach-goma-love-finger-hearts.webp";
});

noBtn.addEventListener( "click" , ( ) => {
    question.innerHTML = "Gapapa kok kalo kamu ga sayang aku, tapi aku selalu sayang sama kamu";
    gif.src = "https://media.tenor.com/s_vTOUd6tx8AAAAi/fbcuteboy.webp";
});