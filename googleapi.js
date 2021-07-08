"use strict";

const GAPI_CLIENT_ID = "CLIENT_ID";

function initAuth(stubs, app, auth) {
  function sendAuthStateChanged() {
    if (auth.isSignedIn.get()) {
      const currentUser = auth.currentUser.get();
      const userId = currentUser.getId();
      const basicProfile = currentUser.getBasicProfile();
      const fullName = basicProfile.getName();
      const email = basicProfile.getEmail();
      const authResponse = currentUser.getAuthResponse();
      const accessToken = authResponse.id_token;
      app.ports.authStateChanged.send({
        userId: userId,
        fullName: fullName,
        email: email,
        accessToken: accessToken,
      });
    } else {
      app.ports.authStateChanged.send({});
    }
  }

  const handlers = {
    signIn: () => {
      if (auth.isSignedIn.get()) {
        sendAuthStateChanged(app);
      } else {
        auth.signIn({ prompt: "login" });
      }
    },
    signOut: () => {
      if (auth.isSignedIn.get()) {
        auth.signOut();
      } else {
        sendAuthStateChanged();
      }
    },
  };

  for (const k in stubs) {
    const handler = handlers[k];
    if (handler) {
      app.ports[k].unsubscribe(stubs[k]);
      app.ports[k].subscribe(() => handler());
    }
  }

  console.log("ports got updated:", app.ports)

  auth.isSignedIn.listen(sendAuthStateChanged);
  sendAuthStateChanged();
}

const stubPorts = (app) => {
  const stubs = {};

  console.log("Got ports:", app.ports)

  for (const k in app.ports) {
    if (app.ports[k].subscribe) {
      stubs[k] = () => {
        throw `No handler for Elm port ${k}`;
      };
      app.ports[k].subscribe(stubs[k]);
    }
  }

  return stubs;
};

window.addEventListener("load", () => {
  const app = Elm.Main.init({
    node: document.getElementById("myapp"),
  });
  const stubs = stubPorts(app);

  new Promise((resolve) => gapi.load("auth2", resolve))
    .then(() =>
      gapi.auth2.init({
        clientId: GAPI_CLIENT_ID,
        scope: "profile https://www.googleapis.com/auth/youtube.readonly",
      })
    )
    .catch((e) => console.error("Error in promiseaaaa: ", e))
    .then(() => initAuth(stubs, app, gapi.auth2.getAuthInstance()))
    .catch((e) => console.error("Error in promise: ", e))
});
