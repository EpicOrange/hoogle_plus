import React from "react";
import { log } from "../utilities/logger";

export const TopBar = () => (
    <div>
	<div style={dlStyle}><small><a href="#" onClick={dlClick}>download log</a></small></div>
    <h1 className="hplus_font">
          <a href="." className="nav-link" id="home">Hoogle+</a>
          <br></br>
          </h1>
    <h5> Welcome to the Hoogle+ Demo </h5>
    <p> {flavorText} </p>
    </div>
);

const flavorText = `
Hoogle+ is a type-driven synthesis engine for Haskell - like Hoogle but able to find compositions of functions. Given a Haskell type, Hoogle+ generates terms that inhabit this type by composing library components. It supports polymorphism, type classes, and higher-order functions. Available library components are listed in the side bar.
`;

const dlStyle = {position:"absolute",
   top:0,
   right:0
}

function dlClick(e) {
	var element = document.createElement('a');
	var msgs = log.exportToArray();
	var text = msgs.join('\n');
    element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
    element.setAttribute('download', 'hoogle-plus.log');

    element.style.display = 'none';
    document.body.appendChild(element);

    element.click();

    document.body.removeChild(element);
}