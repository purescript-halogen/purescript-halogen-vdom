import { JSDOM } from "jsdom";

export function make(content) {
  return () => {
    return new JSDOM(content);
  };
};

export function window(jsdom) {
  return () => {
    return jsdom.window;
  };
};

export function serialize(jsdom) {
  return () => {
    return jsdom.serialize();
  };
};
