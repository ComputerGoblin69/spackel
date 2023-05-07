pub trait Stack {
    type Item: Copy;

    fn push(&mut self, element: Self::Item);

    fn pop(&mut self) -> Self::Item;

    fn dup(&mut self) {
        let v = self.pop();
        self.push(v);
        self.push(v);
    }

    fn swap(&mut self) {
        let b = self.pop();
        let a = self.pop();
        self.push(b);
        self.push(a);
    }

    fn over(&mut self) {
        let b = self.pop();
        let a = self.pop();
        self.push(a);
        self.push(b);
        self.push(a);
    }

    fn nip(&mut self) {
        let b = self.pop();
        self.pop();
        self.push(b);
    }

    fn tuck(&mut self) {
        let b = self.pop();
        let a = self.pop();
        self.push(b);
        self.push(a);
        self.push(b);
    }
}
